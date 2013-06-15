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

xsetroot -name $HOSTNAME
# set the cursor
xsetroot -cursor_name left_ptr

# custom fonts
xset fp+ /usr/share/fonts/local/
xset fp+ /usr/share/fonts/TTF/
xset fp rehash

# turn off the annoying beep
xset b off

# set Spanish keyboard layout
setxkbmap -layout us,es -variant altgr-intl, -option grp:alt_space_toggle -option grp_led:caps -option ctrl:nocaps

# backlight
xbacklight -set 30

# custom X settings
xrdb -merge ~/.Xresources

# composite manager
compton -b --config ~/.compton.conf

##
#  Programs
##

# wicd
#wicd-client -t &

# parcellite
#parcellite &

# notifications
dunst &

# color temperature
gtk-redshift -l 52.50056:13.39889 &

# disable touchpad while typing
syndaemon -t -k -i 2 -d &

# hide cursor when is not needed
unclutter &

# battery indicator
batti &

# caffeine
caffeine -a &

# music player daemon
mpd $HOME/.mpdconf

# scratchpad
tilda --hidden &

# background
$HOME/bin/randombg

# mailnotifier
$HOME/bin/mailnotifier &

##
#  Window manager
##
xrandr --addmode HDMI1 1366x768
xrandr --output HDMI1 --mode 1366x768

exec dwm
