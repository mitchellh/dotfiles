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

  # Install our own
  cd ~/code/dotfiles
  ./install.sh
  cd
fi

#--------------------------------------------------------------------
# Docker

# Fix permissions if they don't allow the Docker user
if [ -S /var/run/docker.sock ]; then
    sudo chgrp docker /var/run/docker.sock
fi

#--------------------------------------------------------------------
# Hostname

# If the FQDN doesn't work then set it manually
if [ "$(hostname -f 2>/dev/null || echo 0)" -eq "0" ]; then
    echo "127.0.0.1     $(hostname)" | sudo tee -a /etc/hosts >/dev/null 2>&1
fi

#--------------------------------------------------------------------
# Start Keybase

# Fix fuse permissions
sudo chmod 0666 /dev/fuse

# Start Keybase
export KEYBASE_NO_SQUIRREL=1
run_keybase >/dev/null

/bin/bash
