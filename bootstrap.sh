#!/bin/zsh
 
echo ">> Synchronizing files"
rsync --exclude ".git/" --exclude ".DS_Store" --exclude "bootstrap.sh" --exclude "README" -av . ~ &> /dev/null

# grab last version of pathogen VIM plugin
mkdir -p ~/.vim/autoload ~/.vim/bundle
curl -so ~/.vim/autoload/pathogen.vim https://raw.github.com/tpope/vim-pathogen/HEAD/autoload/pathogen.vim &

# grab last versions of some pentadactyl plugins
mkdir -p ~/.pentadactyl/plugins
curl -so ~/.pentadactyl/plugins/smooth-scroll.js http://dactyl.sourceforge.net/plugins/smooth-scroll.js &
curl -so ~/.pentadactyl/plugins/flashblock.js http://dactyl.sourceforge.net/plugins/flashblock.js &

source ~/.zshrc
