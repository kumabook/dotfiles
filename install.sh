xcode-select --install

# asdf

if [ -e $HOME/..asdf ]; then
    git clone https://github.com/asdf-vm/asdf.git ~/.asdf
    echo '. $HOME/.asdf/asdf.sh' >> ~/.profile
    echo '. $HOME/.asdf/completions/asdf.bash' >> ~/.profile
fi

asdf plugin-add erlang https://github.com/asdf-vm/asdf-erlang.git
asdf plugin-add elixir https://github.com/asdf-vm/asdf-elixir.git


# install ricty font
cp -f /opt/homebrew/Cellar/ricty/4.1.1/share/fonts/Ricty*.ttf ~/Library/Fonts/
fc-cache -vf
