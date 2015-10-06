xcode-select --install
curl -L git.io/nodebrew | perl - setup

rbenv install 2.2.3
gem install bundler

# install ricty font
cp -f /usr/local/Cellar/ricty/3.2.4/share/fonts/Ricty*.ttf ~/Library/Fonts/
fc-cache -vf
