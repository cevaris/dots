alias ll='ls -la'

# (current_directory)[HH:MM:SS]$
PS1="(\w)[\t]\$ "

# Load local dot files under .local
source ~/.local 2> /dev/null
