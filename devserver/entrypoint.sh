#!/bin/bash

set -e

cd

#--------------------------------------------------------------------
# Install Dotfiles

if [ ! -d "~/code/dotfiles" ]; then
  echo "Cloning dotfiles"
  mkdir -p ~/code
  cd ~/code
  git clone https://github.com/mitchellh/dotfiles.git
  cd

  # Delete all previous dotfiles
  find ~ -maxdepth 1 -type f -name '.[!.]*' -exec rm {} \;

  # Install our own
  cd ~/code/dotfiles
  ./install.sh
  cd
fi

#--------------------------------------------------------------------
# Vim

if [ ! -d ".vim/bundle" ]; then
    mkdir -p .vim .vim/bundle .vim/backup .vim/swap .vim/cache .vim/undo
    curl -fLo .vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    echo "Installing Vim plugins"
    TERM=dumb vim +PlugInstall +qall >vim.log 2>&1
fi

/bin/bash
