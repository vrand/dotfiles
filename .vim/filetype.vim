" filetype specific settings
au FileType python     set omnifunc=pythoncomplete#Complete

" syntax highlighting
au BufRead,BufNewFile /etc/nginx/conf/* set filetype=nginx
au BufRead,BufNewFile Vagrantfile       set filetype=ruby 
au BufRead,BufNewFile */.tmux.conf      set filetype=tmux
au BufRead,BufNewFile */.xmobarrc       set filetype=haskell
au BufRead,BufNewFile ~/.config/uzbl/*  set filetype=uzbl
au BufRead,BufNewFile */uzbl/config     set filetype=uzbl

" xmonad config 
au BufRead xmonad\.hs set makeprg=cp\ %\ ~/.xmonad\ &&\ xmonad\ --recompile
