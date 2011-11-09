#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# moc server
moc &> /dev/null & 

# set the cursor
xsetroot -cursor_name left_ptr

# custom fonts
xset fp+ /usr/share/fonts/local/
xset fp rehash

# turn off the annoying beep
xset b off

# set Spanish keyboard layout
setxkbmap -layout es

# set the background
if [[ -x ~/Images/archlinux.png ]]; then
    feh --bg-scale ~/Images/archlinux.png
else
    xsetroot -solid midnightblue
fi

# custom X settings
xrdb -merge ~/.Xresources

xmonad
