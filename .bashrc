# Reload the shell
function reload {
    source ~/.bash_profile > /dev/null 2>&1
    tmux source-file ~/.tmux.conf > /dev/null 2>&1
}

alias ll='ls -la'

# (current_directory)[HH:MM:SS]$
PS1="(\w)[\t]\$ "

# Load local dot files under .local
source ~/.local 2> /dev/null
