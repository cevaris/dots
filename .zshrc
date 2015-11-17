#!/usr/bin/env zsh

############################################################
# ZSH settings #############################################
export ZSH=/Users/$USER/.oh-my-zsh
source $ZSH/oh-my-zsh.sh
# export TERM=xterm-256color

# Git
export GIT_PROMPT_EXECUTABLE="haskell"
source ~/.zsh.d/zsh-git-prompt/zshrc.sh

# Prompt
function precmd {
    if [[ $PWD/ = ~/workspace/source* ]]; then
	PROMPT="%{$fg[green]%}%c (%{$fg_bold[magenta]%}$(git symbolic-ref --short HEAD)%{$fg[green]%}) %{$fg[red]%}~%{$fg[white]%}࿔ %{$reset_color%}"
	#PROMPT="%{$fg[green]%}%c %{$fg[red]%}~%{$fg[white]%}࿔ %{$reset_color%}"
    else
	PROMPT="%{$fg[green]%}%c $(git_super_status)%{$fg[red]%}~%{$fg[white]%}࿔ %{$reset_color%}"
	if ! [ -z "$VIRTUAL_ENV" ]; then
	    PROMPT="(`basename \"$VIRTUAL_ENV\"`)$PROMPT"
	fi
    fi
}


# Colors
autoload -U colors && colors

# Delete key
bindkey    "^[[3~"          delete-char
bindkey    "^[3;5~"         delete-char

# Make emacs default git editor
export GIT_EDITOR=emacs
export VISUAL=emacs
#export EDITOR=emacs

export HISTSIZE=100000
export HISTFILE="$HOME/.history"
export SAVEHIST=$HISTSIZE
setopt hist_ignore_all_dups

# CD without typing it
setopt autocd
setopt automenu
setopt banghist
# Enables the following cp ^*.(tar|bz2|gz) . 
setopt extendedglob
# Completion
plugins=(completion)

############################################################
# Aliases
alias compose='docker-compose'
alias e='emacs'
alias em='emacs .'
alias emacs='emacs -nw'
alias g='git'
alias hd='hexdump -C'
alias less='less -N'
alias l='less'
alias ll='ls -la'
alias m='man'
alias mci='mvn clean compile  -Denforcer.skip=true'
alias pbsort='pbpaste | sort | pbcopy'
alias r='reload'
alias rm='rm -i'
alias ssh='ssh -v'
alias tmux'TERM=xterm-256color tmux'
alias updatedb='/usr/libexec/locate.updatedb'
alias pants='./pants'
alias jvisualvm="/Applications/VisualVM.app/Contents/MacOS/visualvm"
alias jvis="jvisualvm --openjmx"
alias git-deploy='git co deploy && git pull origin deploy  && git reset --hard origin/deploy'
alias git-master='git co master && git pull origin master  && git reset --hard origin/master'
alias fab='fab --show=debug'
############################################################

# List files
lf() {
    if [ -z "${1}" ]; then
	echo 'Usage:'
	echo 'lf <PATTERN>'
	echo 'lf <PATH> <PATTERN>'
	return
    fi

    if [ -z "${2}" ]; then
	find . -name $1 -type f
    else
	find $1 -name $2 -type f
    fi
}

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

# Date helper
isodate(){
    date -u +"%Y-%m-%dT%H:%M:%SZ"
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
export VIRTUALENVWRAPPER_VIRTUALENV_ARGS='--no-site-packages'
export PIP_VIRTUALENV_BASE=$WORKON_HOME
export PIP_RESPECT_VIRTUALENV=true
source $(which virtualenvwrapper.sh)

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
    export DOCKER_TLS_VERIFY=1}

# Python
export PYTHONDONTWRITEBYTECODE=1

# Ruby
function rvm_init(){

    RVM_VERSION=$(echo $MY_RUBY_HOME | awk -F'-' '{print $2}')
    RVM_GEMSET=$(basename $(PWD))

    for i in "$@"
    do
	case $i in
	    -g=*|--gemset=*)
		RVM_GEMSET="${i#*=}"
		;;
	    -r=*|--ruby=*)
		RVM_VERSION="${i#*=}"
		;;
	    *) # unknown option
		;;
	esac
    done

    print Gemset: $RVM_GEMSET
    print Version: $RVM_VERSION

    if ! type rvm > /dev/null 2>&1; then
	print RVM is not installed
	return 1
    fi
    rvm --create --ruby-version $RVM_VERSION@$RVM_GEMSET
}

# Scala
func scala-init(){
    if [ -z "$1" ]; then
	print Missing project name
	return 1
    fi
    SCALA_PROJ_NAME=$1
    git clone git://github.com/cevaris/skeleton $SCALA_PROJ_NAME
    cd $SCALA_PROJ_NAME
    rm -rf .git
    sed -i '' "s/bonjour/$SCALA_PROJ_NAME/" ./build.sbt
    sbt
}

# Kill by port
func kill-port() {
    _PID_TO_KILL=$(lsof -i :$1 | tail -n 1 | awk '{print $2}')
    echo "Killing port $_PID_TO_KILL"
    kill $_PID_TO_KILL
}

# gitignore
func gitignore() {
    curl https://raw.githubusercontent.com/github/gitignore/master/$1.gitignore
}

# JVX
function jvx() { jvis $1:36001 ;}

#Load Local dot files under .local
source ~/.shell-local 2> /dev/null

export DEPLOY_TAG='SET_ME!!!'

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

# source ~/.bash_profile

# added by travis gem
[ -f /Users/$USER/.travis/travis.sh ] && source /Users/$USER/.travis/travis.sh
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
