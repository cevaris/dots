#!/usr/bin/env zsh

############################################################
# ZSH settings #############################################
export ZSH=/Users/$USER/.oh-my-zsh/
source $ZSH/oh-my-zsh.sh

# completion
plugins=()

# Colors
autoload -U colors && colors
autoload -U +X compinit && compinit
autoload -U +X bashcompinit && bashcompinit

# Delete key
bindkey "^[[3~" delete-char
bindkey "^[3;5~" delete-char

# Make VSCode default git editor
export GIT_EDITOR=code
export EDITOR=code
export VISUAL=code
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
#     PROMPT="$(date "+%H:%M:%S") %{$fg[green]%}%c %{$fg[red]%}~%{$fg[white]%}࿔ %{$reset_color%}"
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

# https://eendroroy.github.io/alien/
source ~/git/zsh-alien/alien.zsh
# export ALIEN_USE_NERD_FONT=1
export ALIEN_THEME="soft"
export ALIEN_SECTION_TIME_FORMAT=%H:%M:%S
export ALIEN_SECTIONS_LEFT=(
  exit
  time
  user
  path
  vcs_branch:async
  # vcs_status:async
  # vcs_dirty:async
  newline
  # ssh
  # venv
  prompt
)

[[ -s ${HOME}/.zshfuncs ]] && source ${HOME}/.zshfuncs # &>/dev/null
[[ -s ${HOME}/.profile ]] && source ${HOME}/.profile

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export LC_ALL="en_US.UTF-8"

# The next line updates PATH for the Google Cloud SDK.
# if [ -f '/Users/acardenas/usr/local/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/acardenas/usr/local/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
# if [ -f '/Users/acardenas/usr/local/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/acardenas/usr/local/google-cloud-sdk/completion.zsh.inc'; fi

export NVM_DIR="$HOME/.nvm"
[ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion


###-tns-completion-start-###
if [ -f /Users/acardenas/.tnsrc ]; then
  source /Users/acardenas/.tnsrc
fi
###-tns-completion-end-###

# python poetry
export PATH="$HOME/.local/bin:$PATH"

# NVM
export PATH="$HOME/.nvm/bin:$PATH"

# pyenv
export PATH="$HOME/.pyenv/shims:$PATH"
[ -s "pyenv" ] && \. eval "$(pyenv init -)"


# rust
PATH="$PATH:$HOME/.cargo/bin"

# Configure OpenSSL for x86 on M1 for SQLite
export LDFLAGS="-L/opt/homebrew/opt/openssl@1.1/lib"
export CPPFLAGS="-I/opt/homebrew/opt/openssl@1.1/include"
export OPENSSL_ROOT=/usr/local/

# WIP Load .nvmrc files automatically
autoload -U add-zsh-hook
load-nvmrc() {
  local node_version="$(nvm version)"
  local nvmrc_path="$(nvm_find_nvmrc)"

  if [ -n "$nvmrc_path" ]; then
    local nvmrc_node_version=$(nvm version "$(cat "${nvmrc_path}")")

    if [ "$nvmrc_node_version" = "N/A" ]; then
      nvm install
    elif [ "$nvmrc_node_version" != "$node_version" ]; then
      nvm use
    fi
  elif [ "$node_version" != "$(nvm version default)" ]; then
    echo "Reverting to nvm default version"
    nvm use default
  fi
}
add-zsh-hook chpwd load-nvmrc
load-nvmrc

# ruby
export PATH="$HOME/.rbenv/shims:$PATH"
eval "$(rbenv init - zsh)"

