set -gx LC_ALL en_US.UTF-8
set -gx LANG en_US.UTF-8

# android
set -gx ANDROID_SDK_ROOT "$HOME/Library/Android/sdk"
set -gx ANDROID_HOME "$ANDROID_SDK_ROOT"
fish_add_path --global --path "$ANDROID_SDK_ROOT/emulator" "$ANDROID_SDK_ROOT/platform-tools" "$ANDROID_SDK_ROOT/cmdline-tools/latest/bin"
# android end

# pnpm
set -gx PNPM_HOME "$HOME/Library/pnpm"
set -gx PNPM_BIN "$PNPM_HOME/bin"
fish_add_path --global --path "$PNPM_BIN"
# pnpm end

fish_add_path --global --path --move \
    $HOME/go/bin \
    $HOME/.local/bin \
    $HOME/dev/other/git-pile/bin \
    /usr/local/bin \
    /opt/homebrew/bin \
    /opt/homebrew/sbin

set -gx RIPGREP_CONFIG_PATH "$HOME/.config/ripgreprc"
set -gx EDITOR nvim

if status is-interactive
    abbr --add emc emacsclient -n
    abbr --add noquar /usr/bin/xattr -r -d com.apple.quarantine

    # C-x C-e to edit current command (Fish also provides Alt-E and Alt-V).
    bind \cx\ce edit_command_buffer
end

if command -q mise
    if status is-interactive
        mise activate fish | source
    else
        mise activate fish --shims | source
    end
end
