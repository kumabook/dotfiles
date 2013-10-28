#!/bin/sh
cd -- "$(dirname -- "${0}")"

ln -s `pwd`/init.el ~/.emacs.d/init.el
ln -s `pwd`/.zshrc  ~/.zshrc
ln -s `pwd`/.screenrc  ~/.screenrc

