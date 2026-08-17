#!/bin/sh

dotfiles_dir="$HOME/dotfiles"

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
mkdir -p "$HOME/.config"
mkdir -p "$HOME/Library/LaunchAgents"
mkdir -p "$HOME/Library/Application Support/lazygit"

link "$dotfiles_dir/vim" "$HOME/.vim"
link "$dotfiles_dir/vim/rc" "$HOME/.vimrc"
link "$dotfiles_dir/zsh/rc" "$HOME/.zshrc"
link "$dotfiles_dir/zsh/profile" "$HOME/.zprofile"
link "$dotfiles_dir/zsh/zshenv" "$HOME/.zshenv"
link "$dotfiles_dir/tig/rc" "$HOME/.tigrc"
link "$dotfiles_dir/ruby/gemrc" "$HOME/.gemrc"
link "$dotfiles_dir/ghostty" "$HOME/.config/ghostty"
link "$dotfiles_dir/fish" "$HOME/.config/fish"
link "$dotfiles_dir/lazygit/config.yml" "$HOME/Library/Application Support/lazygit/config.yml"
link "$dotfiles_dir/bin/mitm_start.sh" "$HOME/.local/bin/mitm_start.sh"
link "$dotfiles_dir/ripgreprc" "$HOME/.config/ripgreprc"
link "$dotfiles_dir/.gitconfig" "$HOME/.gitconfig"
link "$dotfiles_dir/launch-agents/com.oleg.ssh-add-keychain.plist" "$HOME/Library/LaunchAgents/com.oleg.ssh-add-keychain.plist"
