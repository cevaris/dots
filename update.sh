#!/usr/bin/env bash

set -x
ARRAY=(
    .amethyst
#    .emacs
#    .ctags
    .githelpers
    .gitconfig
    .gitignore_global
    .qwerty.txt
    .spacemacs
    .tmux.conf
    .zshrc
    .zshfuncs
)

for i in "${ARRAY[@]}"; do
    ln -sf /git/dots/$i /Users/$USER/$i
done
