#!/bin/bash

current_dir=`pwd`
ESC="\033"
FG_RED=31
FG_GREEN=32
FG_CYAN=36
RED="$ESC[${FG_RED}m"
GREEN="$ESC[${FG_GREEN}m" 
CYAN="$ESC[${FG_CYAN}m" 
RESET="$ESC[0m"

function message {
    echo -e "${CYAN}::${RESET} $1"
}

function ok {
    # $1 program $2 version
    echo -e "\t$1 $2+"
    echo -e "\t=> ${GREEN}OK${RESET}"
}

function error {
    # $1 program $2 min version [$3 installed version]
    UNINSTALLED_ARGS=2

    [ $# -eq 0 ] && exit 1

    echo -e "\t$1 $2+"

    if [ $# -eq $UNINSTALLED_ARGS ] 
    then
        echo -e "\t=> ${RED}MISSING${RESET}"
        return
    fi

    echo -e "\t=> ${RED}CURRENT: $3${RESET}"
}

function dependency {
    local installed=true
    type -p $1 &> /dev/null || installed=false

    if $installed ; then
        local version=$($1 --version | grep -oE -m 1 "[[:digit:]]+\.[[:digit:]]+\.?[[:digit:]]?")
        [[ $version < $2 ]] && error $1 $2 $version || ok $1 $2
    else
        error $1 $2
    fi                      
}

# Check dependencies
message "Checking dependencies"
dependency "git" "1.7"
dependency "vim" "7.3"
dependency "rsync" "2.6"
dependency "xmonad" "0.9"    # X 1.11
 
# Get latest version of the repo
if [ -d ~/dotfiles ]; then
    message "Updating repository"
    cd ~/dotfiles
    git pull origin master &> /dev/null
    git submodule init &> /dev/null
    git submodule update &> /dev/null
else
    message "Downloading repository"
    git clone https://dialelo@bitbucket.org/dialelo/dotfiles ~/dotfiles &> /dev/null
fi

# Install
message "Synchronizing files"
rsync --exclude ".git/" --exclude ".DS_Store" --exclude "bootstrap.sh" --exclude "README" -av . ~ &> /dev/null

mkdir -p ~/.vim/autoload ~/.vim/bundle
curl -so ~/.vim/autoload/pathogen.vim https://raw.github.com/tpope/vim-pathogen/HEAD/autoload/pathogen.vim

mkdir -p ~/.pentadactyl/plugins
curl -so ~/.pentadactyl/plugins/smooth-scroll.js http://dactyl.sourceforge.net/plugins/smooth-scroll.js
curl -so ~/.pentadactyl/plugins/flashblock.js http://dactyl.sourceforge.net/plugins/flashblock.js

# Read settings
message "Reading settings"
source ~/.bash_profile

cd $current_dir
