# History
export HISTSIZE=10000
export HISTFILE="$HOME/.history"

export SAVEHIST=$HISTSIZE

# Editor
export EDITOR=vim
export MYVIMRC=$HOME/dotfiles/vimrc

# Browser
export WWW_HOME=http://dialelo.com
if [[ -z $DISPLAY ]] then
    export BROWSER=elinks
else
    export BROWSER=firefox
fi

# Pager
export PAGER='less -r'
#export MANPAGER="/bin/sh -c 'col -b | vim -u ~/.vim/pager.vim -'"

# Mail
export MAIL=$HOME/.mail/gmail

# Path
function add_to_path
{
    for extra_path in "$@"
    do
        PATH=$PATH:$extra_path
    done
}

ECLIPSE_PATH=/usr/share/eclipse
add_to_path "$ECLIPSE_PATH"

DROPBOX_PATH=$HOME/.dropbox-dist
add_to_path "$DROPBOX_PATH"

SCRIPTS_PATH=$HOME/bin
add_to_path "$SCRIPTS_PATH"

export ANDROID_HOME=/opt/android-sdk
ANDROID_PATH=$ANDROID_HOME/platform-tools
add_to_path "$ANDROID_PATH"

GEMS_PATH=$HOME/.gem/ruby/1.9.1/bin
add_to_path "$GEMS_PATH"

CABAL_PATH=$HOME/.cabal/bin
add_to_path "$CABAL_PATH"

export PATH

# Images
export IMAGES_DIR=$HOME/images

# pip
export PIP_DOWNLOAD_CACHE=$HOME/.pip/cache

# Virtualenvwrapper
export VIRTUALENVWRAPPER_LOG_DIR=/tmp
export VIRTUALENVWRAPPER_PYTHON=$(which python3)

export WORKON_HOME=$HOME/.venvs
#export PROJECT_HOME=$HOME/repos

VENVWRAPPER_SCRIPT=/usr/bin/virtualenvwrapper.sh
if [[ -r $VENVWRAPPER_SCRIPT ]]
then
    source "$VENVWRAPPER_SCRIPT"
fi

# Functions
function modernize
{
    # system
    packer -Syu --noconfirm

    # ruby gems
    gem update

    # python packages
    pip freeze | cut -d = -f 1 | xargs sudo pip install -U

    # haskell
    cabal update
}

function lstash {
    for stash in $(git stash list | awk '{print $1}' | sed -e 's/://'); do
        (echo $stash && git stash show -p $stash);
    done
}

function mkcd {
    mkdir -p $1 && cd $1
}


# Load `z`: https://github.com/rupa/z
. ~/bin/z.sh
