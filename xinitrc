#!/bin/sh
#
# Executed by startx (run your window manager from here)

# Load in the XRDB settings which define how a lot of things
# look (urxvt, emacs, etc.)
xrdb -merge ~/.Xresources

# Make caps lock an additional control key
setxkbmap -option ctrl:nocaps

# Launch the programs
chromium-browser &
emacs &
urxvt -name "irssi" -e "irssi" &

# Start XMonad
exec xmonad
