# History
export HISTSIZE=10000
export HISTFILE="$HOME/.history"

export SAVEHIST=$HISTSIZE

# Editor
export EDITOR=vim
export MYVIMRC=$HOME/dotfiles/.vimrc

# Browser
export WWW_HOME=http://dialelo.com
if [[ -z $DISPLAY ]] then
    export BROWSER=elinks
else
    export BROWSER=uzbl-browser
fi

# Mail
export MAIL=$HOME/Mail

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

# Virtualenvwrapper
export WORKON_HOME=$HOME/.virtualenvs
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
    yaourt -Syu #--no-confirm

    # ruby gems
    gem update

    # python packages
    pip freeze | cut -d = -f 1 | xargs pip install -U

    # haskell packages
    # TODO
}