# The following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion:*' list-colors ''
zstyle :compinstall filename '/home/dialelo/.zshrc'

autoload -Uz compinit
compinit

# History
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

# Misc options
setopt appendhistory 
setopt autocd 
setopt extendedglob 
setopt notify

# Vi key bindings
bindkey -v
