#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

#~/.fehbg &
compton -cCG &
redshift -l 35.3:-120.7 -t 6500:5000 &
wicd-client -t &
xfce4-volumed
#tint2 &
#conky &
synapse -s &
exec awesome
#exec openbox-session
