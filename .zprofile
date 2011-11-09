export EDITOR=vim
export PAGER=less

if [[ -n "$DISPLAY" ]]; then
    export BROWSER=firefox
else
    export BROWSER=links
fi

export PATH=/usr/local/bin:$PATH
