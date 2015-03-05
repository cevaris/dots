#!/usr/bin/env zsh

############################################################
# ZSH settings #############################################

# Extended colors
export TERM='xterm-256color'

# Prompt
# PS1="%{%(#~$fg[red]~$fg[green])%}%n%{$reset_color%}:%/[%*]$ "
PS1='$fg[white]%c$(git_super_status) - %{$reset_color%}'


# Colors
autoload -U colors && colors

# Delete key
bindkey    "^[[3~"          delete-char
bindkey    "^[3;5~"         delete-char


# Make emacs default git editor
export GIT_EDITOR=emacs
#export EDITOR=emacs

export HISTSIZE=100000
export HISTFILE="$HOME/.history"
export SAVEHIST=$HISTSIZE
setopt hist_ignore_all_dups

# CD without typing it
setopt autocd
# Enables the following cp ^*.(tar|bz2|gz) . 
setopt extendedglob
# Completion
source ~/.zsh.d/completion.zsh
# Git
source ~/.zsh.d/zsh-git-prompt/zshrc.sh
# Host completion
local knownhosts
knownhosts=( ${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[0-9]*}%%\ *}%%,*} )
zstyle ':completion:*:(ssh|scp|sftp):*' hosts $knownhosts

############################################################


# Aliases
# alias ls='ls -G'
alias l='less'
alias ll='ls -la'
alias e='emacs'
alias updatedb='/usr/libexec/locate.updatedb'
alias hd='hexdump -C'
############################################################


# Reload the shell
reload() {

    if [[ -f ~/.bash_profile ]] ; then
	source ~/.zshrc
	echo "zsh reloaded."
    fi

}

# Search with line numbers :)
search() {
    grep -inrw $(pwd) -e $1
}

# Validation of puppet recursively
ppv() {
    puppet parser validate $(find /git/puppet -name "*.pp")
}

# Java 
function setjdk() {
  if [ $# -ne 0 ]; then
   removeFromPath '/System/Library/Frameworks/JavaVM.framework/Home/bin'
   if [ -n "${JAVA_HOME+x}" ]; then
    removeFromPath $JAVA_HOME
   fi
   export JAVA_HOME=`/usr/libexec/java_home -v $@`
   export PATH=$JAVA_HOME/bin:$PATH
  fi
}
function removeFromPath() {
  export PATH=$(echo $PATH | sed -E -e "s;:$1;;" -e "s;$1:?;;")
}
setjdk 1.7

# General Path Helper
function pathAdd() {
    if [ -d "$1" ] && [[ ":$PATH:" != *":$1:"* ]]; then
	export PATH="${PATH:+"$PATH:"}$1"
    fi
}

# Python virtualenv
export WORKON_HOME=~/.envs
source /usr/local/bin/virtualenvwrapper.sh

# Go dev
function gopath(){
    export GOPATH=/go
    echo GOPATH=/go
    export GOBIN=$GOPATH/bin
    echo GOBIN=$GOBIN
    pathAdd $GOBIN
}

function docker-ip() {
    boot2docker ip 2> /dev/null
}

function docker-ssh() {
    docker-setup
    boot2docker ssh '[ -f /var/lib/boot2docker/nsenter ] || docker run --rm -v /var/lib/boot2docker/:/target jpetazzo/nsenter'
    args=$@
    boot2docker ssh -t sudo /var/lib/boot2docker/docker-enter "${args[@]}"
}

function docker-setup() {
    boot2docker up
    export DOCKER_HOST=tcp://$(docker-ip):2376
    export DOCKER_CERT_PATH=/Users/$USER/.boot2docker/certs/boot2docker-vm
    export DOCKER_TLS_VERIFY=1    
}



# Python
export PYTHONDONTWRITEBYTECODE=1

#Load Local dot files under .local
source ~/.shell-local 2> /dev/null

