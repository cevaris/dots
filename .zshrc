#!/usr/bin/env zsh

############################################################
# ZSH settings #############################################

autoload -U colors && colors
PS1="%{%(#~$fg[red]~$fg[green])%}%n%{$reset_color%}:%/[%*]$ "
# Delete key
bindkey    "^[[3~"          delete-char
bindkey    "^[3;5~"         delete-char

export HISTSIZE=100000
export HISTFILE="$HOME/.history"
export SAVEHIST=$HISTSIZE
setopt hist_ignore_all_dups

# CD without typing it
setopt autocd
# Enables the following cp ^*.(tar|bz2|gz) . 
setopt extendedglob
# Autocomplete
autoload predict-on
predict-on

############################################################

# Reload the shell
reload() {

    if [[ -f ~/.bash_profile ]] ; then
        #source ~/.bash_profile > /dev/null 2>&1
       source ~/.zshrc
    fi

    if [[ -f ~/.tmux.conf ]] ; then
        #tmux source-file ~/.tmux.conf > /dev/null 2>&1
    fi
}

search() {
    grep -rnw $(pwd) -e $1
}

ppv() {
    puppet parser validate $(find /git/puppet -name "*.pp")
}

# Aliases
alias ll='ls -la'
alias e='emacs'

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

# JAVA_ARGS="${JAVA_ARGS} -Xms1024M -Xmx2048M -XX:PermSize=256m -XX:MaxPermSize=512m"
JAVA_ARGS="-Xms1024M -Xmx2048M -XX:PermSize=256m -XX:MaxPermSize=512m"
SBT_OPTS="${JAVA_ARGS}"
JRUBY_OPTS="-Xms1024M -Xmx2048M -Xcext.enabled=true"
export JAVA_ARGS
export SBT_OPTS
export JRUBY_OPTS


export WORKON_HOME=~/.envs

#Load local dot files under .local
source ~/.local 2> /dev/null

