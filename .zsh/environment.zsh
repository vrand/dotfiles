# misc
setopt autocd

# history
export HISTSIZE=10000
export HISTFILE="$HOME/.history"

export SAVEHIST=$HISTSIZE

setopt hist_ignore_all_dups
setopt hist_ignore_space

export MAIL=$HOME/mail

# virtualenvwrapper configuration
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/src

# TODO: if exists
VENVWRAPPER_SCRIPT=/usr/bin/virtualenvwrapper.sh
source "$VENVWRAPPER_SCRIPT"

##
#  ANDROID_HOME
##

export ANDROID_HOME=/opt/android-sdk

##
#  EDITOR
##

export EDITOR=vim

##
#  BROWSER
##

if [[ -z $DISPLAY ]] then
    export BROWSER=elinks
    export WWW_HOME=http://dialelo.com
else
    export BROWSER=uzbl-browser
fi

##
#  PATH
##

function add_to_path
{
    for extra_path in "$@"
    do
        PATH=$PATH:$extra_path
    done
}

# eclipse
ECLIPSE_PATH=/usr/share/eclipse
add_to_path "$ECLIPSE_PATH" 

# scripts
SCRIPTS_PATH=$HOME/bin
add_to_path "$SCRIPTS_PATH" 

# Android platform tools
ANDROID_PATH=/opt/android-sdk/platform-tools
add_to_path "$ANDROID_PATH" 

# Ruby gems
GEMS_PATH=$HOME/.gem/ruby/1.9.1/bin
add_to_path "$GEMS_PATH" 

# cabal binaries
CABAL_PATH=$HOME/.cabal/bin
add_to_path "$CABAL_PATH" 

export PATH

##
#  FUNCTIONS
##

function modernize
{
    # system
    yaourt -Syu

    # ruby gems
    gem update

    # python packages
    pip freeze | cut -d = -f 1 | xargs pip install -U

    # haskell packages
    # TODO
}
