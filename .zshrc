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

# History
export HISTSIZE=100000
export HISTFILE="$HOME/.history"
export SAVEHIST=$HISTSIZE
setopt APPEND_HISTORY             # Write to history
setopt HIST_EXPIRE_DUPS_FIRST     # Expire duplicate entries first when trimming history.
setopt HIST_FIND_NO_DUPS          # Do not display a line previously found.
setopt HIST_IGNORE_ALL_DUPS       # Delete old recorded entry if new entry is a duplicate.
setopt HIST_IGNORE_DUPS           # Don't record an entry that was just recorded again.
setopt HIST_IGNORE_SPACE          # Don't record an entry starting with a space.
setopt HIST_NO_STORE              # Don't store history commands
setopt HIST_REDUCE_BLANKS         # Remove superfluous blanks before recording entry.
setopt HIST_SAVE_NO_DUPS          # Older duplicates are omitted.
setopt INC_APPEND_HISTORY         # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY              # Share history between all sessions.
SHELL_SESSION_HISTORY=0           # Disable pert-terminal-session

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
alias ag='ag --pager="less -XFR"'
alias csv='column -s, -t -x'
alias dots="code $HOME/git/dots/"
alias emacs="emacs -nw"
alias fmt80='pbpaste | fmt -w 80 | pbcopy'
alias hd='hexdump -C'
alias ll='ls -la'
alias pbsort='pbpaste | sort | uniq | pbcopy'
alias r='reload'
alias rm='rm -i'

alias ssh='TERM=xterm ssh'
alias timeout=gtimeout
alias tmux='TERM=xterm-256color tmux'
alias updatedb='/usr/libexec/locate.updatedb'
alias vnc="echo \"vnc://$(ifconfig | grep "inet 172" | head -n1 | cut -d' ' -f2)\""
############################################################

if [ -f ~/git/zsh-alien/alien.zsh ]; then
  # https://eendroroy.github.io/alien/
  source ~/git/zsh-alien/alien.zsh
  export ALIEN_GIT_SYM=''
  export ALIEN_BRANCH_SYM=''
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
fi

[[ -s ${HOME}/.zshfuncs ]] && source ${HOME}/.zshfuncs # &>/dev/null
[[ -s ${HOME}/.profile ]] && source ${HOME}/.profile

export PATH="$PATH:/opt/homebrew/bin"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export LC_ALL="en_US.UTF-8"

###-tns-completion-start-###
if [ -f /Users/acardenas/.tnsrc ]; then
  source /Users/acardenas/.tnsrc
fi
###-tns-completion-end-###

# python poetry
export PATH="$HOME/.local/bin:$PATH"

# NVM
export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm
[ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion
# Load .nvmrc files automatically
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

# pyenv
export PATH="$HOME/.pyenv/shims:$PATH"
[ -s "pyenv" ] && \. eval "$(pyenv init -)"

# rust
export PATH="$PATH:$HOME/.cargo/bin"

# Configure OpenSSL for x86 on M1 for SQLite
export LDFLAGS="-L/opt/homebrew/opt/openssl@1.1/lib"
export CPPFLAGS="-I/opt/homebrew/opt/openssl@1.1/include"
export OPENSSL_ROOT=/usr/local/

# ruby
# export PATH="$HOME/.rbenv/shims:$PATH"
# eval "$(rbenv init - zsh)"

[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
export PATH="/opt/homebrew/bin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"export PATH="/opt/homebrew/opt/libpq/bin:$PATH"
