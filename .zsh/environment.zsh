# misc
setopt autocd

# history
export HISTSIZE=10000
export HISTFILE="$HOME/.history"

export SAVEHIST=$HISTSIZE

setopt hist_ignore_all_dups
setopt hist_ignore_space

# virtualenvwrapper configuration
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/src
source /usr/bin/virtualenvwrapper.sh

# editor
export EDITOR=vim

# browser
if [[ -n $DISPLAY ]] then
    export BROWSER=elinks
else
    export BROWSER=firefox
fi
export WWW_HOME=http://dialelo.com

# path
export PATH=$HOME/bin:/usr/local/bin/android-sdk-linux/platform-tools:/usr/local/bin/android-sdk-linux/tools:$HOME/.cabal/bin:$PATH
