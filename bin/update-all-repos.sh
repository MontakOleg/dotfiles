#!/usr/bin/env bash
set -uo pipefail

usage() {
  cat <<'USAGE'
Usage: ./update-all-repos.sh [-j workers] [-d scan_depth] [target]

Recursively find git repositories under target and run:
  git pull --rebase

Notes:
  - Directories named .build are always excluded from scanning.
  - Repositories with marker file .update-skip are skipped.
  - SSH connections are shared per (ssh command, host) for the whole run.

Options:
  -j workers      Number of parallel workers (default: 16)
  -d scan_depth   Max depth for .git marker search from target root (default: 3)
  -h              Show this help

Examples:
  ./update-all-repos.sh
  ./update-all-repos.sh -j 10
  ./update-all-repos.sh -j 6 -d 4 backend
USAGE
}

workers=16
scan_depth=3
target="."
marker_file=".update-skip"

while getopts ":j:d:h" opt; do
  case "$opt" in
    j)
      workers="$OPTARG"
      ;;
    d)
      scan_depth="$OPTARG"
      ;;
    h)
      usage
      exit 0
      ;;
    :)
      echo "Option -$OPTARG requires a value" >&2
      usage >&2
      exit 1
      ;;
    \?)
      echo "Unknown option: -$OPTARG" >&2
      usage >&2
      exit 1
      ;;
  esac
done
shift $((OPTIND - 1))

if [[ $# -gt 1 ]]; then
  echo "Too many arguments" >&2
  usage >&2
  exit 1
fi

if [[ $# -eq 1 ]]; then
  target="$1"
fi

if ! [[ "$workers" =~ ^[1-9][0-9]*$ ]]; then
  echo "Invalid workers value: $workers (expected positive integer)" >&2
  exit 1
fi

if ! [[ "$scan_depth" =~ ^[1-9][0-9]*$ ]]; then
  echo "Invalid scan depth value: $scan_depth (expected positive integer)" >&2
  exit 1
fi

if ! target_abs="$(cd "$target" 2>/dev/null && pwd -P)"; then
  echo "Target directory not found: $target" >&2
  exit 1
fi

echo "Starting update: target=$target_abs, workers=$workers, scan_depth=$scan_depth"
echo "Scanning for git repositories (excluding .build, marker=$marker_file)..."

declare -a repos=()
declare -a skipped_repos=()

while IFS= read -r -d '' git_marker; do
  repo="${git_marker%/.git}"

  # Submodule checkouts have .git as a file pointing into .git/modules.
  if [[ -f "$git_marker" ]] && grep -qE '(^|/)\.git/modules/' "$git_marker"; then
    continue
  fi

  if [[ -f "$repo/$marker_file" ]]; then
    skipped_repos+=("$repo")
    continue
  fi

  repos+=("$repo")
done < <(
  find "$target_abs" -maxdepth "$scan_depth" \
    \( -type d -name .build -prune \) -o \
    \( -type d -name .git -print0 -prune \) -o \
    \( -type f -name .git -print0 \)
)

if ((${#repos[@]} == 0)); then
  if ((${#skipped_repos[@]} > 0)); then
    echo "No repositories to update: all found repositories are skipped by marker $marker_file"
  else
    echo "No git repositories found under: $target_abs"
  fi
  exit 0
fi

total=${#repos[@]}

# No mapfile: macOS /bin/bash is 3.2.
declare -a sorted=() worktrees=()
while IFS= read -r -d '' repo; do
  if [[ -f "$repo/.git" ]] && grep -q '/worktrees/' "$repo/.git"; then
    worktrees+=("$repo")
  else
    sorted+=("$repo")
  fi
done < <(printf '%s\0' "${repos[@]}" | sort -zu)
repos=(${sorted[@]+"${sorted[@]}"})

if ((${#skipped_repos[@]} > 0)); then
  echo "Skipped by marker $marker_file: ${#skipped_repos[@]}"
fi

echo "Found $total repositories. Running updates..."

# Repo's own ssh command plus a connection shared by repos with the same command,
# so repos pinned to different keys (core.sshCommand -i ...) never share one.
ssh_command() {
  local cmd
  cmd="${GIT_SSH_COMMAND:-$(git -C "$1" config --get core.sshCommand || echo ssh)}"
  printf '%s -o ControlMaster=auto -o ControlPath=%s/%s-%%C -o ControlPersist=60' \
    "$cmd" "$MUX_DIR" "$(printf '%s' "$cmd" | cksum | cut -d ' ' -f 1)"
}

# Prints "<ssh command>\t<destination>" if origin is reached over SSH.
ssh_target() {
  local url host
  url="$(git -C "$1" remote get-url origin 2>/dev/null)" || return 0
  case "$url" in
    ssh://*) host="${url#ssh://}"; host="${host%%/*}" ;;
    *://*) return 0 ;;
    *:*) host="${url%%:*}" ;;
    *) return 0 ;;
  esac
  printf '%s\tssh://%s\n' "$(ssh_command "$1")" "$host"
}

update_repo() {
  local repo="$1" out reason
  GIT_SSH_COMMAND="$(ssh_command "$repo")"
  export GIT_SSH_COMMAND
  if out="$(git -C "$repo" pull --rebase --quiet 2>&1)"; then
    printf 'OK\t%s\n' "$repo" >> "$STATUS_FILE"
    echo "OK   $repo"
  else
    # First meaningful line, without hints and ssh multiplexing chatter.
    reason="$(printf '%s\n' "$out" |
      grep -v -e '^ControlSocket ' -e '^mux_client_' -e '^hint: ' -e '^Auto-merging ' |
      grep -m 1 '[^[:space:]]' | sed 's/^[[:space:]]*//' | tr '\t' ' ')"
    reason="${reason:-git pull failed}"
    printf 'FAIL\t%s\t%s\n' "$repo" "$reason" >> "$STATUS_FILE"
    echo "FAIL $repo: $reason"
  fi
}
export -f ssh_command ssh_target update_repo

status_file="$(mktemp)"
# Short path: unix socket paths are limited to ~104 bytes, macOS TMPDIR is too long.
mux_dir="$(mktemp -d /tmp/update-repos.XXXXXX)"
cleanup() {
  local sock
  for sock in "$mux_dir"/*; do
    [[ -S "$sock" ]] && ssh -o ControlPath="$sock" -O exit _ >/dev/null 2>&1
  done
  rm -rf "$mux_dir" "$status_file"
}
trap cleanup EXIT
export STATUS_FILE="$status_file" MUX_DIR="$mux_dir"

# Open shared connections up front: otherwise workers handshake all at once,
# and GitLab drops more than ~10 concurrent SSH handshakes.
echo "Opening shared SSH connections..."
while IFS=$'\t' read -r cmd dest; do
  sh -c "$cmd -o BatchMode=yes -o ConnectTimeout=10 -fN \"\$1\"" _ "$dest" </dev/null >/dev/null 2>&1 &
done < <(printf '%s\0' ${repos[@]+"${repos[@]}"} | xargs -0 -n1 -P "$workers" bash -c 'ssh_target "$1"' _ | sort -u)
wait

run_updates() {
  local jobs="$1"
  shift
  (($# > 0)) || return 0
  printf '%s\0' "$@" | xargs -0 -n1 -P "$jobs" bash -c 'update_repo "$1"' _
}
run_updates "$workers" ${repos[@]+"${repos[@]}"}
# Worktrees share refs with their main checkout: parallel fetches race for ref locks.
run_updates 1 ${worktrees[@]+"${worktrees[@]}"}

ok_count="$(awk -F '\t' '$1=="OK" {c++} END {print c+0}' "$status_file")"
fail_count="$(awk -F '\t' '$1=="FAIL" {c++} END {print c+0}' "$status_file")"

echo "Done. Total: $total, OK: $ok_count, FAIL: $fail_count"

if ((fail_count > 0)); then
  echo "Failed repositories:"
  awk -F '\t' '$1=="FAIL" {print " - " $2 ": " $3}' "$status_file" | sort
  exit 1
fi
