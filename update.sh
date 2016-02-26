#!/usr/bin/env bash

set -x
ARRAY=(
    .emacs
    .ctags
    .local.bash
    .tmux.conf
    .zshrc
)

for i in "${ARRAY[@]}"; do
    ln -sf /git/dots/$i /Users/$USER/$i
done
