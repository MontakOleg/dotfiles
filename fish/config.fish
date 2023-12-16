if status is-interactive
    # Commands to run in interactive sessions can go here
end

# Setup brew
eval "$(/opt/homebrew/bin/brew shellenv)"

# Setup rtx
~/bin/rtx activate fish | source

