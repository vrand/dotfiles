" filetype specific settings
autocmd FileType python set omnifunc=pythoncomplete#Complete
autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
autocmd FileType css set omnifunc=csscomplete#CompleteCSS

" syntax highlighting
au BufRead,BufNewFile /etc/nginx/conf/* set filetype=nginx
au BufRead,BufNewFile Vagrantfile       set filetype=ruby 
au BufRead,BufNewFile */.tmux.conf      set filetype=tmux
au BufRead,BufNewFile */.xmobarrc       set filetype=haskell
au BufRead,BufNewFile ~/.config/uzbl/*  set filetype=uzbl
au BufRead,BufNewFile */uzbl/config     set filetype=uzbl

" xmonad config 
au BufRead xmonad\.hs set makeprg=cp\ %\ ~/.xmonad\ &&\ xmonad\ --recompile

" automatically reload .vimr when it's saved
au BufWritePost .vimrc source ~/dotfiles/.vimrc

" Makefiles depend on tabs to work 
autocmd FileType make setlocal noexpandtab

" dissable paste mode when leaving Insert Mode
au InsertLeave * set nopaste
