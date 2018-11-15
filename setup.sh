#!/bin/sh
cd -- "$(dirname -- "${0}")"

ln -s `pwd`/.vimrc ~/.vimrc
ln -s `pwd`/.zshrc  ~/.zshrc
ln -s `pwd`/.screenrc  ~/.screenrc
ln -s `pwd`/Brewfile ~/Brewfile
ln -s `pwd`/.tmux.conf ~/.tmux.conf
ln -s `pwd`/.gitconfig ~/.gitconfig
ln -s `pwd`/.gitignore ~/.gitignore

mkdir ~/.emacs.d/
ln -s `pwd`/init.el ~/.emacs.d/init.el
ln -s `pwd`/Cask ~/.emacs.d/Cask
