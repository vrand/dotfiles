# History
export HISTFILE=$HOME/.zsh_history
export HISTSIZE=10000
export SAVEHIST=10000

# Options
setopt APPEND_HISTORY
setopt AUTO_CD
setopt CDABLE_VARS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_REDUCE_BLANKS
setopt INTERACTIVE_COMMENTS
setopt MARK_DIRS
setopt NOTIFY
setopt NOHUP

# Environment
export EDITOR=vim
export PAGER=less

if [[ -n "$DISPLAY" ]]; then
    export BROWSER=firefox
else
    export BROWSER=links
fi

export PATH=/usr/local/bin:$PATH
