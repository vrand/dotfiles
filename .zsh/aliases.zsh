# Directory tree navigation
alias -g ..='cd ..'
alias -g ...='cd ../..'

# Most visited dirs
alias blog='cd ~/src/blog'
alias isp='cd ~/src/ISP'
alias video='cd /mnt/data/video'

# Shortcuts
alias jklocal='jekyll --auto --pygments --server --base-url "/" . _site'
alias cl=clear
alias -g g=git
alias ll='ls -lA'
alias mkdir='mkdir -p'
alias pyclean='find . -name "*.pyc" -exec rm {} \;'
alias s=sudo
alias tm='tmux attach'
alias -g v=vim
alias yao=yaourt

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
alias -g NUL='> /dev/null 2>&1'
