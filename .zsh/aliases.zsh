# Directory tree navigation
alias -g ..='cd ..'
alias -g ...='cd ../..'
alias -g ....='cd ../../..' 
alias -g .....='cd ../../../..' 

# Most visited dirs
alias video='cd /data/video'
alias music='cd /data/music'
alias books='cd /data/books'
alias src='cd ~/repos'
alias blog='cd ~/blog'
alias dot='cd ~/dotfiles'
alias battery='acpi | grep -o "...%"'

# repos
alias mut='cd ~/repos/Mutualismo && workon m'
alias tur='cd ~/repos/turses && workon t'

# Shortcuts
alias a='alsamixer'
alias e='emacsclient -tc'
alias d=deactivate
alias g=git
alias ll='ls -lA'
alias lh='ls -lAh'
alias m='mux'
alias -g mkdir='mkdir -p'
alias s=sudo
alias t=tmux
alias tu=turses
alias v=vim
alias vi=vim
alias x='cd && xinit'
alias w=workon
alias mt='make && turses'
alias z='zathura --fork'

# Useful
alias manage='python manage.py'
alias pyclean='find -name "*.pyc" -exec rm {} \;'
alias networks='sudo iw wlan0 scan | grep -o "SSID:.*" | cut -d " " -f 2 | sort -iu'
alias jklocal='jekyll --auto --pygments --server --base-url "/" . _site'
alias mostused='cat ~/.history | sort | uniq -c | sort -nr | head -n 10 | nl' 
alias wallpaper='feh --bg-scale $IMAGES_DIR/`ls $IMAGES_DIR | sort -R | tail -n 1`'

# Django
alias djtest='python manage.py test'
alias djserver='python manage.py runserver'

# yaourt
alias y=yaourt
alias yu='yaourt -Syu'
alias yi='yaourt -S'
alias ys='yaourt -Ss'
alias yr='yaourt -Rdd'

# pip
alias pips='pip search'
alias -g pipi='pip install'
alias -g pipu='pip uninstall'

# Files by suffix
alias -s c=$EDITOR
alias -s com=$BROWSER
alias -s h=$EDITOR
alias -s html=$BROWSER
alias -s java=$EDITOR
alias -s jpg=feh
alias -s js=$EDITOR
alias -s org=$BROWSER
alias -s png=feh
alias -s py=python
alias -s PKGBUILD=$EDITOR

# Useful
alias -g G='| grep'
alias -g L='| less'
alias -g DN='&> /dev/null'
alias -g CAT='| xargs cat |'
