#!/bin/sh
cd -- "$(dirname -- "${0}")"

ln -s `pwd`/.vimrc ~/.vimrc
ln -s `pwd`/.zshrc  ~/.zshrc
ln -s `pwd`/.screenrc  ~/.screenrc
ln -s `pwd`/.pentadactylrc  ~/.pentadactylrc
ln -s `pwd`/.pentadactyl  ~/.pentadactyl
ln -s `pwd`/Brewfile ~/Brewfile

mkdir ~/.emacs.d/
ln -s `pwd`/init.el ~/.emacs.d/init.el
ln -s `pwd`/Cask ~/.emacs.d/Cask
