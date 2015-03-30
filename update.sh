#!/usr/bin/env bash

set -x
ARRAY=(
    .emacs
    .zshrc
    .zsh.d/zsh-git-prompt/zshrc.sh
    .ctags
)

for i in "${ARRAY[@]}"; do
    ln -sf /git/dots/$i /Users/$USER/$i
done
