#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

xrdb -merge ~/.Xresources
setxkbmap -option ctrl:nocaps
exec xmonad
