#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

# Make caps lock an additional control key
xmodmap ~/.Xmodmap
xrdb -merge ~/.Xresources
exec xmonad
#exec gnome-session