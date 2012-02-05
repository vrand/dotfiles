# Directory tree navigation
alias -g ..='cd ..'
alias -g ...='cd ../..'
alias -g ....='cd ../../..' 
alias -g .....='cd ../../../..' 

# Most visited dirs
alias blog='cd ~/blog'
alias video='cd /mnt/data/video'
alias books='cd /mnt/data/books'
alias src='cd ~/repos'

# Shortcuts
alias cl=clear
alias -g g=git
alias ll='ls -lA'
alias mkdir='mkdir -p'
alias s=sudo
alias tm='tmux attach'
alias -g v=vim
alias vi=vim
alias wk=workon

# Useful
alias manage='python manage.py'
alias djtest='python manage.py test'
alias djserver='python manage.py runserver'
alias pyclean='find . -name "*.pyc" -exec rm {} \;'
alias networks='sudo iw wlan0 scan | grep -o "SSID:.*" | cut -d " " -f 2 | sort -iu'
alias jklocal='jekyll --auto --pygments --server --base-url "/" . _site'
alias mostused='cat ~/.history | sort | uniq -c | sort -nr' 

# yaourt
alias y=yaourt
alias yu='yaourt -Syu'
alias yi='yaourt -S'
alias ys='yaourt -Ss'
alias yr='yaourt -Rdd'

# pip
alias pips='pip search'
alias pipi='pip install'
alias pipu='pip uninstall'

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
alias -g LC='| wc -l'
alias -g DN='/dev/null'
alias -g L='| less'
alias -g NUL='&> /dev/null'
