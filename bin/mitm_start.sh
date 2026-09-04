#!/bin/bash

set -euo pipefail

interface="Wi-Fi"
proxy_command=mitmproxy
if [[ $# -eq 1 && $1 == "--web" ]]; then
  proxy_command=mitmweb
elif [[ $# -ne 0 ]]; then
  echo "Usage: ${0##*/} [--web]" >&2
  exit 2
fi
if ! command -v "$proxy_command" >/dev/null; then
  echo "$proxy_command is not installed." >&2
  exit 127
fi

# Read both protocols before changing anything. networksetup cannot retrieve
# proxy passwords, so authenticated proxies cannot be safely restored here.
proxy_types=(web secureweb)
proxy_servers=()
proxy_ports=()
proxy_states=()
for proxy_type in "${proxy_types[@]}"; do
  settings=$(networksetup "-get${proxy_type}proxy" "$interface")
  server=$(sed -n 's/^Server: //p' <<< "$settings")
  port=$(sed -n 's/^Port: //p' <<< "$settings")
  enabled=$(sed -n 's/^Enabled: //p' <<< "$settings")
  authenticated=$(sed -n 's/^Authenticated Proxy Enabled: //p' <<< "$settings")
  if [[ $authenticated != 0 ]]; then
    echo "Cannot restore credentials for the $proxy_type proxy on $interface." >&2
    exit 1
  fi
  case "$enabled" in
    Yes) state=on ;;
    No) state=off ;;
    *) echo "Cannot read the $proxy_type proxy state on $interface." >&2; exit 1 ;;
  esac
  proxy_servers+=("$server")
  proxy_ports+=("$port")
  proxy_states+=("$state")
done

proxy_pid=
cleanup() {
  local status=$? i
  trap - EXIT
  trap '' INT TERM
  set +e
  if [[ -n $proxy_pid ]]; then
    kill -TERM "$proxy_pid" 2>/dev/null
  fi
  echo "Restoring proxy settings on $interface"
  for i in "${!proxy_types[@]}"; do
    # An unconfigured proxy has an empty server and port 0; restore only its
    # disabled state, as networksetup requires an endpoint when setting one.
    if [[ -n ${proxy_servers[$i]} ]]; then
      networksetup "-set${proxy_types[$i]}proxy" "$interface" \
        "${proxy_servers[$i]}" "${proxy_ports[$i]}" off || status=1
    fi
    networksetup "-set${proxy_types[$i]}proxystate" "$interface" \
      "${proxy_states[$i]}" || status=1
  done
  # Restore networking first, then let the proxy finish its terminal cleanup.
  if [[ -n $proxy_pid ]]; then
    wait "$proxy_pid" 2>/dev/null
  fi
  exit "$status"
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

echo "Setting proxy on $interface"
networksetup -setwebproxy "$interface" localhost 8080 off
networksetup -setsecurewebproxy "$interface" localhost 8080 off

# wait is interruptible, so SIGTERM restores settings even while mitmproxy runs.
# Keep stdin attached for mitmproxy's terminal UI.
"$proxy_command" <&0 &
proxy_pid=$!
wait "$proxy_pid"
proxy_pid=
