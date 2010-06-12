# Mitchell Hashimoto's Dot Files

These are the various config files I have to setup a system
the way I want it.

## Installation

My dotfiles are organized such that they physically all exist in a
`~/.dotfiles` directory, and are then symlinked to my actually `$HOME`
directory.

    git clone git://github.com/mitchellh/dotfiles.git ~/.dotfiles
    cd ~/.dotfiles
    rake install
