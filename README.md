# Mitchell Hashimoto's Dot Files

These are the various config files I have to setup a system
the way I want it.

## Installation

    git clone git://github.com/mitchellh/dotfiles.git ~/.dotfiles
    cd ~/.dotfiles

Then use the `thor` task to install whatever you want. The example
below installs the bash dotfiles:

    thor dotfiles:install bash

Any file or path that matches the name will be installed. So
`thor dotfiles:install emacs` will install all emacs files, for
example.

## Specific Dotfile Docs

### Emacs

My emacs folder has a fairly sane layout, but there are a couple things
you should be aware of if you choose to use it:

* The SYSTEMTYPE.el (e.g. `darwin.el`) is loaded for your specific system,
  if the file exists. So `darwin.el` is only loaded on Macs. This is important
  to me since I often switch back and forth between Mac/Linux.
* The USERNAME.el (e.g. `mitchellh.el`) is loaded for your specific username
  on your system. This allows you to put in user-specific modifications.

A couple load path variables are setup to make things easy:

* `dotfiles-dir` - This variable contains the path to `.emacs.d`. You can use
  this to easily `concat` and add more load paths.
* `packages-dir` - This variable contains the path to `dotfiles-dir/packages`,
  useful for easily adding new packages to the load path.
