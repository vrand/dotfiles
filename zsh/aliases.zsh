# Directory tree navigation
alias -g ..='cd ..'
alias -g ...='cd ../..'
alias -g ....='cd ../../..'
alias -g .....='cd ../../../..'

# Shortcuts
alias a='alsamixer'
alias cc='noglob cclive'
alias d=deactivate
alias e='emacsclient -tc'
alias es='setxkbmap -layout es'
alias g=git
alias gr=grunt
alias h='noglob http'
alias i=ipython
alias irc=weechat-curses
alias k=killall
alias l='ls'
alias ll='ls -lAh'
alias m='mux'
alias n='noglob'
alias off='sudo systemctl poweroff'
alias news=newsbeuter
alias -g mkdir='mkdir -p'
alias pod=podbeuter -a
# *r*epo *r*oot
alias rr='cd $(git rev-parse --show-toplevel)'
alias s=sudo
alias sus='sudo systemctl suspend'
alias this='tmux attach -t $(basename $PWD)'
alias t=tmux
alias tu='turses -d'
alias us='setxkbmap -layout us'
alias v=vim
alias vi=vim
alias va=vagrant
alias x='cd && xinit'
alias w=workon
alias mt='make && turses'
alias setbg='feh --bg-scale'
alias self='mplayer tv:// -tv driver=v4l2:width=640:height=480:device=/dev/video0 -fps 15 -vf screenshot'
alias so=source
alias sys='sudo systemctl'
alias za='zathura --fork'

# Useful
alias pyclean='find -name "*.pyc" -exec rm {} \;'
alias networks='sudo iw wlan0 scan | grep -o "SSID:.*" | cut -d " " -f 2 | sort -iu'
alias jklocal='jekyll --auto --pygments --server --base-url "/" . _site'
alias mostused='cat ~/.history | sort | uniq -c | sort -nr | head -n 10 | nl'

# packer
alias y=packer
alias yu='packer -Syu --noconfirm'
alias yi='packer -S --noconfirm'
alias ys='packer -Ss'
alias yr='packer -Rdd'

# Python
alias p=python2
alias p3=python3
alias serve='python3 -m http.server'

# pip
alias pips='pip search'
alias -g pipi='pip install'
alias -g pipu='pip uninstall'
alias pipf='pip freeze'

# pypy
alias py='pypy'
alias pyp='/opt/pypy/bin/pip'

# external screens
alias dualscreen='xrandr --output HDMI1 --mode 1280x1024 --right-of LVDS1'
alias samescreen='xrandr --output HDMI1 --mode $(xrandr | grep LVDS1 | cut -d " " -f 3 | cut -d "+" -f 1) --same-as LVDS1'

# Django
alias pm='python manage.py'

# Files by suffix
alias -s c=$EDITOR
alias -s com=$BROWSER
alias -s h=$EDITOR
alias -s html=$BROWSER
alias -s java=$EDITOR
alias -s jpg=feh
alias -s org=$BROWSER
alias -s png=feh
alias -s py=python
alias -s PKGBUILD=$EDITOR

# Common piping idioms
alias -g G='| grep'
alias -g L='| less'
alias -g LC='| wc -l'
alias -g DN='&> /dev/null'
alias -g CAT='| xargs cat |'
alias -g JSON='| python -m json.tool'

# Common args
alias -g H='--help '
alias -g V='--version'
