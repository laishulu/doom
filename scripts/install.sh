#!/usr/bin/env bash

if [[ $(uname) == 'Darwin' ]]; then
  brew tap d12frosted/emacs-plus
  brew install emacs-plus
  brew tap laishulu/homebrew
  brew install macism
fi

# required dependencies
brew install git ripgrep
# optional dependencies
brew install coreutils fd
# Installs clang
xcode-select --install

# needed by vterm of emacs
brew install cmake libvterm

brew install editorconfig gnu-indent global

rm -rf ~/.config/doom
rm -rf ~/.config/emacs
mkdir -p ~/.config
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
git clone https://github.com/laishulu/doom.git ~/.config/doom
~/.config/emacs/bin/doom install
