#!/bin/sh

autorandr.py --change
xsetroot -cursor_name left_ptr -solid "#202020"
xset r rate 400 20
xset b off
xset +fp ~/.local/share/fonts
[[ -f ~/.config/Xresources ]] && xrdb -merge ~/.config/Xresources

redshift &
xfce4-volumed &
xscreensaver -no-splash &

export _JAVA_AWT_WM_NONREPARENTING=1

exec xmonad
# exec ~/src/wtftw/target/wtftw
