#!/bin/bash

DOTFILES=`pwd`
cd ~
mkdir .config

ln -s ${DOTFILES}/.gitconfig ~
cat ${DOTFILES}/.ssh/authorized_keys >> ~/.ssh/authorized_keys
ln -s ${DOTFILES}/.tmux.conf ~
ln -s ${DOTFILES}/fish ~/.config/fish

cd $DOTFILES
