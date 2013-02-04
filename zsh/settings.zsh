# cd into a directory typing its name
setopt autocd

# ignore duplicates in history
setopt hist_ignore_all_dups
# ignore leading and trailing whitespace in history
setopt hist_ignore_space

# don't allow overwriting existing files with `>`, forces
# you to use `>|` instead
setopt noclobber
# have `|` added by default in history entries
setopt hist_allow_clobber
