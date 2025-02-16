if status is-interactive
    # Commands to run in interactive sessions can go here
end

abbr --add emc emacsclient -n

set -gx LC_ALL en_US.UTF-8
set -gx LANG en_US.UTF-8

set -gx PATH \
    /Users/me/.local/share/mise/shims \
    /Users/me/go/bin \
    /Users/me/.local/bin \
    /Users/me/.mint/bin \
    /Users/me/dev/other/git-pile/bin \
    /usr/local/bin \
    /opt/homebrew/bin \
    $PATH

# Setup mise
~/.local/bin/mise activate fish | source

