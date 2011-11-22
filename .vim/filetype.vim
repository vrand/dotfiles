" filetype specific settings
au FileType python     set omnifunc=pythoncomplete#Complete

" syntax highlighting
au BufRead,BufNewFile /etc/nginx/conf/* set filetype=nginx
au BufRead,BufNewFile Vagrantfile set filetype=ruby 
