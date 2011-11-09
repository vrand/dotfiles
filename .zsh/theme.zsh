local user='%{$fg[magenta]%}%n%{$fg[white]%}@%{$fg[cyan]%}%m%{$reset_color%}'
local pwd='%{$fg[white]%}[%{$bg[blue]$fg[yellow]%}%~%{$reset_color%}%{$fg[white]%}]'
local datetime='%{$bg[white]$fg[black]%}%T $(date +%a\ %d.%m.%y)%{$reset_color%}'

PROMPT="${user}${pwd}%# "
RPROMPT="${datetime}"
