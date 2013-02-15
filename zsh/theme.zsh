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
    echo '»'
}

local user='%{$fg[cyan]%}%n%{$reset_color%}'
local hostname='%{$fg[yellow]%}%m%{$reset_color%}'
local cwd='%{$fg[magenta]%}%~%{$reset_color%}'
local git_branch='%{$bg[red]%}%{$fg[white]%}$(current_branch)%}%{$reset_color%}'
local virtualenv='%{$bg[yellow]%}%{$fg[black]%}$(current_virtualenv)%}%{$reset_color%}'
#local datetime='%{$bg[white]${fg[black]%}%T $(date +%a\ %d.%m.%y)%{$reset_color%}'
#local battery='%{$fg[red]%}$(~/bin/battery)%{$reset_color%}'
local prompt_char='$(prompt_char)'

PROMPT="${user} @ ${hostname} in ${cwd} ${prompt_char} "
RPROMPT="${git_branch} ${virtualenv}"
