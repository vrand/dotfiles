# ls colors
autoload colors; colors;
export LSCOLORS="Gxfxcxdxbxegedabagacad"

# Enable ls colors
if [ "$DISABLE_LS_COLORS" != "true" ]
then
  # Find the option for using colors in ls, depending on the version: Linux or BSD
  ls --color -d . &>/dev/null 2>&1 && alias ls='ls --color=tty' || alias ls='ls -G'
fi

setopt no_beep
setopt auto_cd
setopt multios
setopt cdablevarS

if [[ x$WINDOW != x ]]
then
    SCREEN_NO="%B$WINDOW%b "
else
    SCREEN_NO=""
fi

# Setup the prompt with pretty colors
setopt prompt_subst

function current_branch() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo ${ref#refs/heads/}
}

function current_virtualenv() {
    [[ -x $VIRTUAL_ENV ]] || return
    echo $VIRTUAL_ENV | grep -o "\w*$"
}

# Taken from Steve Losh's great article on tuning the ZSH prompt:
# http://stevelosh.com/blog/2010/02/my-extravagant-zsh-prompt/
function prompt_char {
    git branch >/dev/null 2>/dev/null && echo '±' && return
    #hg root >/dev/null 2>/dev/null && echo '☿' && return
    echo 'λ'
}

# show vim status alongside right prompt
# http://zshwiki.org/home/examples/zlewidgets
function zle-line-init zle-keymap-select {
    # info
    git_branch='%{$bg[red]%}%{$fg[white]%}$(current_branch)%}%{$reset_color%}'
    virtualenv='%{$bg[yellow]%}%{$fg[black]%}$(current_virtualenv)%}%{$reset_color%}'
    #hostname='%{$bg[blue]%}%m%{$reset_color%}'
    info="${git_branch} ${virtualenv}"

    # vi mode
    mode="${${KEYMAP/vicmd/-- NORMAL --}/(main|viins)/-- INSERT --}"
    if [[ "$mode" == "-- NORMAL --" ]]
    then
        cmode="%{$fg[cyan]%}$mode%{$reset_color%}"
    else
        cmode="%{$fg[yellow]%}$mode%{$reset_color%}"
    fi

    # prompt!
    RPS1="$info $cmode"
    RPS2=$RPS1
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

#local user='%{$fg[yellow]%}%n%{$reset_color%}'
local cwd='%{$fg[magenta]%}%~%{$reset_color%}'
#local datetime='%{$bg[white]${fg[black]%}%T $(date +%a\ %d.%m.%y)%{$reset_color%}'
#local battery='%{$fg[red]%}$(~/bin/battery)%{$reset_color%}'
local prompt_char='%{$fg[cyan]%}$(prompt_char)'

PROMPT="${cwd} ${prompt_char} "
