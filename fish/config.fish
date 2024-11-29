if status is-interactive
    # Commands to run in interactive sessions can go here
end

abbr --add emc emacsclient -n

set -gx LC_ALL en_US.UTF-8
set -gx LANG en_US.UTF-8

# Setup mise
~/.local/bin/mise activate fish | source

