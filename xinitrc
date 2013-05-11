#!/bin/bash

if [ -f "$HOME/.profile" ]; then
	. "$HOME/.profile"
fi

xset b off
xsetroot -cursor_name left_ptr -solid \#4b4b4b
xmodmap $HOME/.xmodmaprc
xrandr --output LVDS1 --mode 1366x768 --pos 0x0

xfsettingsd
xfce4-power-manager &
xfce4-volumed &
$HOME/bin/mytrayer &
nm-applet &
blueman-applet &
redshift -l 35.3:-120.7 -t 6500:5000 &
pulseaudio --start
synapse -s &
Thunar --daemon &
xscreensaver -no-splash &
unclutter &

exec xmonad # startxfce4

