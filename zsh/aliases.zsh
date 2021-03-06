# Mail
alias mp='mutt -e "source ~/.mutt/personal.muttrc"'
alias mw='mutt -e "source ~/.mutt/work.muttrc"'

# Directory tree navigation
alias -g ..='cd ..'
alias -g ...='cd ../..'
alias -g ....='cd ../../..'
alias -g .....='cd ../../../..'

# Shortcuts
alias :e=vim
alias a='alsamixer'
alias cc='noglob cclive'
alias d=deactivate
alias e='emacsclient -tc'
alias es='translate {=es}'
alias g=git
alias lolgit='git commit -m "$(shuf -n 1 ~/.emoji)"'
alias gr=grunt
alias h='noglob http'
alias i=ipython
alias irc=weechat-curses
alias k=killall
alias kbplug='setxkbmap -layout us,es -variant altgr-intl, -option grp:alt_space_toggle -option grp_led:caps -option ctrl:nocaps'
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
alias try='mktmpenv && pip install '
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

# copy-paste
alias y='xclip -i '
alias p='xclip -o'

# Useful
alias pyclean='find -name "*.pyc" -exec rm {} \;'
alias networks='sudo iw wlan0 scan | grep -o "SSID:.*" | cut -d " " -f 2 | sort -iu'
alias jklocal='jekyll --auto --pygments --server --base-url "/" . _site'
alias mostused='cat ~/.history | sort | uniq -c | sort -nr | head -n 10 | nl'

# packer
alias yu='packer -Syu --noconfirm'
alias yi='packer -S --noconfirm'
alias ys='packer -Ss'
alias yr='packer -Rdd'
alias yclean='sudo pacman -Scc && sudo pacman -Rs $(pacman -Qtdq)'

# Python
alias py=python2
alias py3=python3
alias serve='python3 -m http.server'

# pip
alias pips='pip search'
alias pipi='pip install'
alias pipu='pip uninstall'
alias pipf='pip freeze'

# pypy
alias pyp='/opt/pypy/bin/pip'

# external screens
alias dualscreen='xrandr --output HDMI1 --mode 1280x1024 --right-of LVDS1'
alias samescreen='xrandr --output HDMI1 --mode $(xrandr | grep LVDS1 | cut -d " " -f 3 | cut -d "+" -f 1) --same-as LVDS1'

# Django
alias pm='python manage.py'

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
