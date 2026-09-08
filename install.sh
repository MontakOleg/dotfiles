#!/bin/sh

set -eu

dotfiles_dir="$HOME/dotfiles"

mise_cmd=$(command -v mise || true)
if [ -z "$mise_cmd" ]; then
    mise_cmd="$HOME/.local/bin/mise"
fi
"$mise_cmd" -C "$dotfiles_dir" bootstrap dotfiles apply

# Symlink src -> dst, replacing an existing symlink but never clobbering
# anything real. Re-running is safe.
link() {
    src="$1"
    dst="$2"

    if [ -e "$dst" ] && [ ! -L "$dst" ]; then
        echo "skip: $dst exists and is not a symlink" >&2
        return
    fi

    ln -sfn "$src" "$dst"
}

mkdir -p "$HOME/.local/bin"
mkdir -p "$HOME/Library/LaunchAgents"

link "$dotfiles_dir/bin/mitm_start.sh" "$HOME/.local/bin/mitm_start.sh"
link "$dotfiles_dir/launch-agents/com.oleg.ssh-add-keychain.plist" "$HOME/Library/LaunchAgents/com.oleg.ssh-add-keychain.plist"
