# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Autocompletion
complete -cf man
complete -cf sudo

# History
HISTCONTROL=ignoreboth  # avoid duplicate or blank commands on history
HISTFILESIZE=10000      # large history
HISTSIZE=10000
shopt -s histappend     # append history rather than overwrite

# Misc
IGNOREEOF=1             # Ctrl-d twice to exit
shopt -s nocaseglob     # case insensitive globbing

# Other settings
for dotfile in $HOME/.{bash_prompt,aliases,functions,exports,inputrc}
do
    [ -r "$dotfile" ] && source "$dotfile"
done
