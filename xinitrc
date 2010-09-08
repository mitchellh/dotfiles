#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

{ sleep 5; xmodmap ~/.Xmodmap; } &
xrdb -merge ~/.Xresources
exec xmonad
