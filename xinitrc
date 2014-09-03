#!/bin/sh

~/bin/autorandr
xsetroot -cursor_name left_ptr -solid "#202020" &
xset r rate 400 20 &
xset b off &

redshift &
xfce4-volumed &
thunar --daemon &
xscreensaver -no-splash &

exec xmonad
