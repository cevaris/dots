#!/usr/bin/env bash

set -x
ARRAY=(
    .emacs
    .ctags
    .githelpers
    .gitignore_global
    .tmux.conf
    .zshrc
    .zshfuncs
)

for i in "${ARRAY[@]}"; do
    ln -sf /git/dots/$i /Users/$USER/$i
done
