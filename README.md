# Dotfiles

macOS configuration with Fish as the main interactive shell and zsh for agent
commands. Clone this repository to `~/dotfiles`.

## Installation

Install Homebrew and mise first. mise is managed separately from Homebrew;
make `mise` available on `PATH` for the commands below.

```sh
brew bundle --file="$HOME/dotfiles/Brewfile"
mise trust "$HOME/dotfiles/mise.toml"
mise -C "$HOME/dotfiles" bootstrap --dry-run
mise -C "$HOME/dotfiles" bootstrap
```

`mise.toml` declares the application and shell config links, the macOS
preferences, the SSH LaunchAgent and the Neovim plugin install. `mise bootstrap`
applies them all and installs the tools from the global mise config. mise reports
conflicting config targets without overwriting them; resolve those conflicts
before rerunning.
Vim and Neovim share `vim/rc`; FZF's binary comes from Homebrew and its Vim
integration from vim-plug. Oh My Zsh is optional.

## Shell environment

- `fish/config.fish` configures Fish; abbreviations and key bindings are interactive.
- `zsh/env` preserves inherited paths and puts mise shims first without running
  subprocesses. `.zshenv` loads it for every invocation; `.zprofile` reloads it
  after macOS and Homebrew reorder `PATH`, including for `zsh -lc`.
- `zsh/rc` contains interactive aliases, completions and mise activation.

## Proxy helper

`mitm_start.sh` runs mitmproxy (`--web` selects mitmweb) for the Wi-Fi service.
The helper restores previously
configured HTTP/HTTPS endpoints and their enabled state on exit, including
SIGINT and SIGTERM. Previously unconfigured proxies are disabled on exit.
Authenticated proxies are left untouched because their passwords cannot be read
back by `networksetup`.
