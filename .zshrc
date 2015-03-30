#!/usr/bin/env zsh

############################################################
# ZSH settings #############################################
export ZSH=/Users/adamc/.oh-my-zsh
source $ZSH/oh-my-zsh.sh

# Git
source ~/.zsh.d/zsh-git-prompt/zshrc.sh

# Prompt
function precmd {
    PROMPT="%{$fg[green]%}%c $(git_super_status)%{$fg[red]%}~%{$fg[white]%}࿔ %{$reset_color%}"
    if ! [ -z "$VIRTUAL_ENV" ]; then
	PROMPT="(`basename \"$VIRTUAL_ENV\"`)$PROMPT"
    fi
}


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
plugins=(completion)

############################################################
# Aliases
alias e='emacs'
alias em='emacs .'
alias g='git'
alias hd='hexdump -C'
alias l='less'
alias ll='ls -la'
alias m='man'
alias rm='rm -i'
alias updatedb='/usr/libexec/locate.updatedb'


############################################################ 

# Sample file
samplef() {
    # set -x
    if [ -z "${1}" ]; then
	echo 'Error: Missing file path'
	echo
	echo 'Usage:'
	echo 'samplef <FILEPATH> <SAMPLE_SIZE>'
	echo 
	echo 'Optional paramters: <SAMPLE_SIZE>, default is 0.1'
	echo 'ex; samplef ./myfile.txt 0.25'
	return
    fi
    SAMPLE_RATIO=${2:-0.1}
    cat $1 | perl -n -e "print if (rand() < $SAMPLE_RATIO)"
}


ctags-emacs(){
    ctags -e -R .
    print Done indexing $(pwd)
}

ctags-python(){
    PYTHON_VENV=${$(which python)/\/bin\/python/}
    ctags -e -R . $PYTHON_VENV
}

# Reload the shell
reload() {
    if [[ -f ~/.zshrc ]] ; then
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
alias goplay='cd /go/src/github.com/cevaris'
export GOPATH=/go
export GOBIN=$GOPATH/bin
pathAdd $GOBIN

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

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
