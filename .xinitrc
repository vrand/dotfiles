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

# set the cursor
xsetroot -cursor_name left_ptr

# custom fonts
xset fp+ /usr/share/fonts/local/
xset fp rehash

# turn off the annoying beep
xset b off

# set Spanish keyboard layout
setxkbmap -layout es

# custom key mappings
xmodmap ~/.Xmodmap

# set the background
if [[ -f $HOME/images/wallpaper ]]; then
    feh --bg-scale $HOME/images/wallpaper
else
    xsetroot -solid midnightblue
fi

# xflux
xflux -l 55.859531 -g 9.846015

# backlight
xbacklight -set 50

# tray
trayer --edge top --align right --widthtype percent --width 5 --height 22 \
      --transparent true  --alpha 0 --tint black --padding 5 --SetDockType true &

# custom X settings
xrdb -merge ~/.Xresources

# terminal
urxvt &

xmonad
