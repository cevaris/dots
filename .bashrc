function reload {
    source ~/.bash_profile 2> /dev/null
    echo ".....reloaded shell"
}

alias ll='ls -la'
alias tms='tmux attach || tmux new'

em() { emacs $1; } 

PS1="(\w)[\t]\$ "

source ~/.local 2> /dev/null
export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
