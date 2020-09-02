#!/usr/bin/env bash

set -x
ARRAY=(
    .amethyst
    .githelpers
    .gitconfig
    .gitignore_global
    .qwerty.txt
    .tmux.conf
    .zshrc
    .zshfuncs
)

for i in "${ARRAY[@]}"; do
    ln -sf /Users/$USER/git/dots/$i /Users/$USER/$i
done
