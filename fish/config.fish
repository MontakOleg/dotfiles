if status is-interactive
    # Commands to run in interactive sessions can go here
end

abbr --add emc emacsclient -n
abbr --add noquar /usr/bin/xattr -r -d com.apple.quarantine

set -gx LC_ALL en_US.UTF-8
set -gx LANG en_US.UTF-8

# android
set -gx ANDROID_SDK_ROOT "$HOME/Library/Android/sdk"
set -gx ANDROID_HOME "$ANDROID_SDK_ROOT"
fish_add_path -g "$ANDROID_SDK_ROOT/emulator" "$ANDROID_SDK_ROOT/platform-tools" "$ANDROID_SDK_ROOT/cmdline-tools/latest/bin"
# android end

# pnpm
set -gx PNPM_HOME "/Users/me/Library/pnpm"
if not string match -q -- $PNPM_HOME $PATH
  set -gx PATH "$PNPM_HOME" $PATH
end
# pnpm end

set -gx PATH \
    # /Users/me/.local/share/mise/shims \
    /Users/me/go/bin \
    /Users/me/.local/bin \
    /Users/me/dev/other/git-pile/bin \
    /usr/local/bin \
    /opt/homebrew/bin \
    $PATH

set -gx EDITOR nvim

# C-x C-e to edit current command
# By default fish uses alt-e and alt-v
bind \cx\ce edit_command_buffer

# Setup mise
~/.local/bin/mise activate fish | source
