#!/usr/bin/env bash

# Reload the shell
reload() {

    if [[ -f ~/.bash_profile ]] ; then
        source ~/.bash_profile > /dev/null 2>&1
    fi

    if [[ -f ~/.tmux.conf ]] ; then
        tmux source-file ~/.tmux.conf > /dev/null 2>&1
    fi
}

alias ll='ls -la'

# (current_directory)[HH:MM:SS]$
PS1="(\w)[\t]\$ "

# Load local dot files under .local
source ~/.local 2> /dev/null
