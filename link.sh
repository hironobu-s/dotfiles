#!/bin/bash

DOTFILES=`pwd`
cd ~

ln -s ${DOTFILES}/.emacs.d ~
ln -s ${DOTFILES}/.git ~
ln -s ${DOTFILES}/.gitconfig ~
ln -s ${DOTFILES}/.ssh/authorized_keys ~/.ssh/authorized_keys
ln -s ${DOTFILES}/.tmux.conf ~
ln -s ${DOTFILES}/.zshrc ~

cd $DOTFILES
