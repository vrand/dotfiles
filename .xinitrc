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

## 
#  X
##

# set the cursor
xsetroot -cursor_name left_ptr

# custom fonts
xset fp+ /usr/share/fonts/local/
xset fp+ /usr/share/fonts/TTF/
xset fp rehash

# turn off the annoying beep
xset b off

# set Spanish keyboard layout
setxkbmap -layout es

# custom key mappings
xmodmap ~/.Xmodmap

# xflux
#xflux -l 36.725384 -g -4.414939 # Málaga
#xflux -l 43.131057 -g -2.548485  # Elorrio
xflux -l 52.50056 -g 13.39889 # Berlin

# backlight
xbacklight -set 50

# custom X settings
xrdb -merge ~/.Xresources

# xcompmgr
xcompmgr -cCfF -D 4 &

## 
#  Programs
##

# wicd
wicd-client -t &

# parcellite
parcellite &

## 
#  Window manager
##

exec awesome
