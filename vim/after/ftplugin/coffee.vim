setlocal shiftwidth=2 tabstop=2 expandtab
autocmd BufWritePost *.coffee silent CoffeeMake!
