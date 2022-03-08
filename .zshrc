#!/usr/bin/env zsh

############################################################
# ZSH settings #############################################
export ZSH=/Users/$USER/.oh-my-zsh/
source $ZSH/oh-my-zsh.sh

# Completion
plugins=(completion)

# Colors
autoload -U colors && colors
autoload -U +X compinit && compinit
autoload -U +X bashcompinit && bashcompinit

# Git prompt
autoload -U promptinit
promptinit
PURE_CMD_MAX_EXEC_TIME=1
PURE_PROMPT_SYMBOL="%{$fg[red]%}~%{$fg[white]%}࿔%{$reset_color%}"
PURE_GIT_PULL=0
PURE_GIT_UNTRACKED_DIRTY=0
prompt pure

# Delete key
bindkey "^[[3~" delete-char
bindkey "^[3;5~" delete-char

# Make emacs default git editor
export GIT_EDITOR=emacs
export EDITOR=emacs
export VISUAL=emacs
export HISTSIZE=100000
export HISTFILE="$HOME/.history"
export SAVEHIST=$HISTSIZE

export COMPLETION_WAITING_DOTS="true"

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

# function precmd {
#     source /opt/twitter/opt/git/etc/bash_completion.d/git-prompt.sh
#     if [[ $(__git_ps1 "%s") = *[!\ ]* ]]; then
# 	PROMPT="%{$fg[green]%}%c (%{$fg_bold[magenta]%}$(__git_ps1 "%s")%{$fg[green]%}) %{$fg[red]%}~%{$fg[white]%}࿔ %{$reset_color%}"
#     else
# 	PROMPT="%{$fg[green]%}%c %{$fg[red]%}~%{$fg[white]%}࿔ %{$reset_color%}"
#     fi
# }
# function precmd {
#     PROMPT="%{$fg_bold[magenta]%}$(date "+%H:%M:%S") ❯  %{$reset_color%}"
# }

############################################################
# Aliases
# alias arc='arc --trace'
alias ag='ag --pager="less -XFR"'
alias compose='docker-compose'
alias csv='column -s, -t -x'
alias dots="code $HOME/git/dots/"
alias emacs="emacs -nw"
alias fab='fab --show=debug'
alias fmt80='pbpaste | fmt -w 80 | pbcopy'
#alias gci='git ci -am'
#alias gm='git co master && git pull origin master'
#alias gpom='git push origin master'
alias hd='hexdump -C'
alias jvis="jvisualvm --openjmx"
alias jvisualvm="/Applications/VisualVM.app/Contents/MacOS/visualvm"
alias ll='ls -la'
alias m='man'
alias mci='mvn clean compile  -Denforcer.skip=true'
#alias pants='./pants'
alias pbsort='pbpaste | sort | uniq | pbcopy'
alias py.test'py.test -s'
alias rake='noglob rake'
alias r='reload'
alias rm='rm -i'

alias ssh='TERM=xterm ssh'
alias timeout=gtimeout
alias tmux'TERM=xterm-256color tmux'
alias updatedb='/usr/libexec/locate.updatedb'
alias vnc="echo \"vnc://$(ifconfig | grep "inet 172" | head -n1 | cut -d' ' -f2)\""
############################################################

[[ -s ${HOME}/.zshfuncs ]] && source ${HOME}/.zshfuncs # &>/dev/null
[[ -s ${HOME}/.profile ]] && source ${HOME}/.profile
[[ -s ${HOME}/workspace/tweetypie-sandbox/dotfiles/.profile ]] && source ${HOME}/workspace/tweetypie-sandbox/dotfiles/.profile

export EE_PANTS_DAEMON_BETA=0

export PATH=$PATH:/usr/local/cassandra/bin
export PATH=$PATH:/usr/local/elasticsearch/bin
export PATH=$PATH:/usr/local/hadoop/bin
export PATH=$PATH:/usr/local/kafka/bin
export PATH=$PATH:/usr/local/mongodb/bin
export PATH=$PATH:/usr/local/zookeeper/bin
export PATH=$PATH:/Applications/Postgres.app/Contents/Versions/9.4/bin
export PATH=$PATH:/Applications/Sublime\ Text.app/Contents/SharedSupport/bine
#export PATH=$PATH:/go/bin
#export PATH=$PATH:$HOME/Library/Python/2.7/bin

export GOPATH=/go
export GOBIN=$GOPATH/bin
export PATH=$GOBIN/bin:$PATH

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export PATH="/usr/local/opt/mysql@5.6/bin:$PATH"
export PATH="$HOME/.yarn/bin:$PATH"
export PATH="$HOME/.fastlane/bin:$PATH"
export PATH="$HOME/Library/Python/2.7/bin:$PATH"
export LESS="--RAW-CONTROL-CHARS -N"

export LC_ALL="en_US.UTF-8"

# android dev
export ANDROID_HOME=$HOME/Library/Android/sdk
export ANDROID_SDK_ROOT=$HOME/Library/Android/sdk
export ANDROID_SDK_HOME=$HOME/Library/Android/sdk
export ANDROID_EMULATOR_HOME=$HOME/.android
export ANDROID_AVD_HOME=$HOME/.android/avd
export PATH=${PATH}:$HOME/Library/Android/sdk/platform-tools
export PATH=${PATH}:$HOME/Library/Android/sdk/tools
export PATH=${PATH}:$HOME/Library/Android/sdk/tools/bin

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/acardenas/usr/local/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/acardenas/usr/local/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/acardenas/usr/local/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/acardenas/usr/local/google-cloud-sdk/completion.zsh.inc'; fi

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"                   # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && . "$NVM_DIR/bash_completion" # This loads nvm bash_completion

###-tns-completion-start-###
if [ -f /Users/acardenas/.tnsrc ]; then
  source /Users/acardenas/.tnsrc
fi
###-tns-completion-end-###

## rbenv
export PATH="$HOME/.rbenv/shims:$PATH"
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"
