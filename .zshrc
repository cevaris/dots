#!/usr/bin/env zsh

############################################################
# ZSH settings #############################################
export ZSH=/Users/$USER/.oh-my-zsh/
source $ZSH/oh-my-zsh.sh

# Completion
plugins=()

# Colors
autoload -U colors && colors

# Delete key
bindkey "^[[3~"  delete-char
bindkey "^[3;5~" delete-char

# Make emacs default git editor
export GIT_EDITOR=emacs
export VISUAL=emacs
#export EDITOR=emacs
export HISTSIZE=100000
export HISTFILE="$HOME/.history"
export SAVEHIST=$HISTSIZE

setopt extended_history
setopt hist_ignore_all_dups
setopt histignorespace
setopt auto_cd
setopt automenu
setopt banghist
setopt correct
setopt no_clobber
setopt chase_dots
setopt extendedglob
setopt share_history

function precmd {
    source /opt/twitter/opt/git/etc/bash_completion.d/git-prompt.sh
    if [[ $(__git_ps1 "%s") = *[!\ ]* ]]; then
	PROMPT="%{$fg[green]%}%c (%{$fg_bold[magenta]%}$(__git_ps1 "%s")%{$fg[green]%}) %{$fg[red]%}~%{$fg[white]%}࿔ %{$reset_color%}"
    else
	PROMPT="%{$fg[green]%}%c %{$fg[red]%}~%{$fg[white]%}࿔ %{$reset_color%}"
    fi
}

export GIT_TAG='SET_ME!!!'
export GIT_TAG='dataproducts/deploy-tag-20160218-090236'

############################################################
# Aliases
alias compose='docker-compose'
alias csv='column -s, -t -x'
alias e='emacs'
alias em='emacs .'
alias emacs='emacs -nw'
alias fab='fab --show=debug'
alias g='git'
alias gci='git ci -am'
alias git-master='git co master && git pull origin master'
alias hd='hexdump -C'
alias jvis="jvisualvm --openjmx"
alias jvisualvm="/Applications/VisualVM.app/Contents/MacOS/visualvm"
alias l='less'
alias less='less -N'
alias ll='ls -la'
alias m='man'
alias mci='mvn clean compile  -Denforcer.skip=true'
alias pants='./pants'
alias pbsort='pbpaste | sort | uniq | pbcopy'
alias py.test'py.test -s'
alias python='ipython'
alias r='reload'
alias rm='rm -i'
alias ssh='ssh -v'
alias tmux'TERM=xterm-256color tmux'
alias updatedb='/usr/libexec/locate.updatedb'
############################################################

[[ -s ${HOME}/.zshfuncs ]] && source ${HOME}/.zshfuncs &>/dev/null
