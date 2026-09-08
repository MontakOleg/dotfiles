# Dotfiles

macOS configuration with Fish as the main interactive shell and zsh for agent
commands. Clone this repository to `~/dotfiles`.

## Installation

Install Homebrew and mise first. mise is managed separately from Homebrew;
make `mise` available on `PATH` for the commands below.

```sh
brew bundle --file="$HOME/dotfiles/Brewfile"
mise trust "$HOME/dotfiles/mise.toml"
mise -C "$HOME/dotfiles" bootstrap dotfiles apply --dry-run
~/dotfiles/install.sh
nvim +PlugInstall
```

`mise.toml` declares the application and shell config links. `install.sh` applies
them through mise, then links the proxy helper and SSH LaunchAgent. mise reports
conflicting config targets without overwriting them; resolve those conflicts
before rerunning. For the two remaining links, the installer replaces symlinks
but skips existing regular files and directories.
Vim and Neovim share `vim/rc`; FZF's binary comes from Homebrew and its Vim
integration from vim-plug. Oh My Zsh is optional. `setup_mac.sh` separately applies
the Dock and input preferences.

## Shell environment

- `fish/config.fish` configures Fish; abbreviations and key bindings are interactive.
- `zsh/env` preserves inherited paths and puts mise shims first without running
  subprocesses. `.zshenv` loads it for every invocation; `.zprofile` reloads it
  after macOS and Homebrew reorder `PATH`, including for `zsh -lc`.
- `zsh/rc` contains interactive aliases, completions and mise activation.

## Proxy helper

`mitm_start.sh` runs mitmproxy (`--web` selects mitmweb) for the Wi-Fi service.
Install mitmproxy separately before using it. The helper restores previously
configured HTTP/HTTPS endpoints and their enabled state on exit, including
SIGINT and SIGTERM. Previously unconfigured proxies are disabled on exit.
Authenticated proxies are left untouched because their passwords cannot be read
back by `networksetup`.
