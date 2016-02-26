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
utc(){
    date -u
}
isodate(){
    date -u +"%Y-%m-%dT%H:%M:%SZ"
}
lexicaldate(){
    date -u +"%Y%m%d%H%M%S"
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

func git-tag(){
    if [ -z "$1" ]; then
	print Missing tag prefix
	return 1
    fi
    _DATE=$(lexicaldate)
    _TAG_NAME="$1-$_DATE"
    echo "creating tag $_TAG_NAME"
    git tag -a $_TAG_NAME -m "$1"
    echo "pushing tag $_TAG_NAME"
    git push origin $_TAG_NAME
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

[[ -s ${HOME}/.local.bash ]] && source ${HOME}/.local.bash &>/dev/null
