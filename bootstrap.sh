#!/bin/bash

current_dir=`pwd`

function dependency {
    local installed=true
    type -p $1 &> /dev/null || installed=false

    if $installed ; then
        local version=$($1 --version | grep -oE -m 1 "[[:digit:]]+\.[[:digit:]]+\.?[[:digit:]]?")
        [[ $version < $2 ]] && local msg="$1 version installed: $version, version needed: $2"
    else
        local msg="$1 missing"
    fi                      

    if ! $installed || [ -n "$msg" ] ; then
        missing+=($msg)
    fi
}

# Check dependencies
echo ":: Checking dependencies"
dependency "git" "1.7"
dependency "vim" "7.3"
dependency "rsync" "2.6"
dependency "xmonad" "0.9"    # X 1.11
 
if [ "${#missing[@]}" -gt "0" ]; then
    echo "ERROR: Missing dependencies"
    for dep in "${missing[@]}"; do
        echo "$dep."
    done
    exit 1
fi

# Get latest version of the repo
if [ -d ~/dotfiles ]; then
    echo ":: Updating repository"
    cd ~/dotfiles
    git pull origin master &> /dev/null
    git submodule init &> /dev/null
    git submodule update &> /dev/null
else
    echo ":: Downloading repository"
    git clone https://dialelo@bitbucket.org/dialelo/dotfiles ~/dotfiles &> /dev/null
fi

# Install
echo ":: Synchronizing files"
rsync --exclude ".git/" --exclude ".DS_Store" --exclude "bootstrap.sh" --exclude "README" -av . ~ &> /dev/null

mkdir -p ~/.vim/autoload ~/.vim/bundle
curl -so ~/.vim/autoload/pathogen.vim https://raw.github.com/tpope/vim-pathogen/HEAD/autoload/pathogen.vim

mkdir -p ~/.pentadactyl/plugins
curl -so ~/.pentadactyl/plugins/smooth-scroll.js http://dactyl.sourceforge.net/plugins/smooth-scroll.js
curl -so ~/.pentadactyl/plugins/flashblock.js http://dactyl.sourceforge.net/plugins/flashblock.js

# Read settings
echo ":: Reading settings"
source ~/.bash_profile

cd $current_dir
