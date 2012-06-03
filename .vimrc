filetype off

" Vundle
set runtimepath+=~/.vim/bundle/vundle
call vundle#rc()

" Bundles
" ~~~~~~~

" TODO: sort bundles by category

" from GitHub
Bundle 'mileszs/ack.vim'
Bundle 'altercation/vim-colors-solarized'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-repeat'
Bundle 'scrooloose/nerdcommenter'
Bundle 'scrooloose/nerdtree'
Bundle 'scrooloose/syntastic'
Bundle 'jmcantrell/vim-virtualenv'
Bundle 'peplin/ropevim'
Bundle 'godlygeek/tabular'
Bundle 'Lokaltog/vim-powerline'

" from www.vim.org/scripts
Bundle 'nginx.vim'
Bundle 'Gundo'
Bundle 'pep8'
Bundle 'pydoc.vim'
Bundle 'vimwiki'
Bundle 'scratch'
Bundle 'L9'
Bundle 'FuzzyFinder'
Bundle 'Color-Sampler-Pack'
Bundle 'YankRing.vim'
Bundle 'SuperTab-continued.'
Bundle 'ZoomWin'

filetype plugin on

" General
" ~~~~~~~

" Vim settings rather than Vi settings
set nocompatible

set nobackup
set noswapfile
"set backupdir=~/.vim/backup
"set directory=~/.vim/swap

" save undo history for each file
set undodir=~/.vim/undo
set undofile
set undolevels=1000

" encoding
set encoding=utf-8

" <Space> is easier to type than "\"
let mapleader = " "

" redraw instead of insert/delete
set ttyfast

" abbreviate messages
set shortmess=a

" enable autocompletion menu
set wildmenu
set wildmode=list:longest:full

set wildignore+=*.o,*.obj,*.pyc
set wildignore+=.git
set wildignore+=*.dvi,*.pdf
set wildignore+=*.jpg,*.png,*.tiff

" tells VIM where to search for autocompletion
"  . : current file
"  w : files in other windows
"  b : files in loaded buffers, not in a window
"  t : the `tags` file
"  i : current and included files
set complete=.,w,b,t,i

" autocompletion visualization
set completeopt=menuone,longest,preview

" Aesthetics
" ~~~~~~~~~~

function! SetColorScheme()
    if &t_Co != 256
        colorscheme xemacs
    else
        let g:solarized_termcolors=256
        colorscheme solarized
    endif
endfunc


syntax on
call SetColorScheme()
set background=dark

nmap <leader>tb :call ToggleBackground()<CR>
function! ToggleBackground()
    if &background == "dark"
        set background=light
        call SetColorScheme()
    else
        set background=dark
    endif
endfunc


" UI
" ~~

" last window always has a status line
set laststatus=2

" show information about the cursor coords and relative position
set ruler

" highlight current line and column
set cursorline
set cursorcolumn

"" wrap long lines
set wrap
set textwidth=79
"set formatoptions=tcqn

"" visual indicator in 80-th column
set colorcolumn=80

if has('gui_running')
    set guifont=Deja\ Vu\ Sans\ Mono\ 14
endif

" Editing
" ~~~~~~~

" show invisible chars (*t*oggle *l*ist)
nmap <leader>tl :set list!<CR>
set list
set listchars=tab:▸\ ,eol:¬,trail:·,extends:↷,precedes:↶

" *t*oggle *s*pelling / switch language
nmap <leader>ts :set spell!<CR>
set spelllang=es_es
nmap <leader>su :set spelllang=en_us<CR>
nmap <leader>sg :set spelllang=en_gb<CR>

" show relative line number
set relativenumber

" read changes to files outside Vim
set autoread

" 4 spaces instead of <Tab>
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab

" indentation behaviour
set autoindent
set copyindent

" folding
set foldmethod=indent

" move the cursor across empty characters
set virtualedit=all

" allow backspacing over everything on insert mode
set backspace=indent,eol,start

" swap cases with ~
set tildeop

" keep the current line always in the center of the buffer
set scrolloff=99

" put a '$' at the end of the text to change while making changes
set cpoptions+=$

" Search
" ~~~~~~

" incremental searching
set incsearch

"" wrap the bottom and top of the file while searching
set wrapscan

"" case insensitive search unless pattern contains upper case characters
set ignorecase
set smartcase

"" highlight all matches with the search pattern
set hlsearch

" global flag automatically on with the substitute command
set gdefault

" Commands
" ~~~~~~~~

" save as superuser
command! Sudow :w !sudo tee % &> /dev/null<CR>

" remove trailing whitespaces in the whole file
command! -nargs=* StripTrailingWhitespace :%s/\s\+$//
map <Leader>st :StripTrailingWhitespace<CR>''

" avoid common mistakes
cmap W w
cmap Q q
cmap WQ wq
cmap Wq wq
cmap wQ wq
cmap Sp sp
cmap Sb sb

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
if !exists(":DiffOrig")
    command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
                \ | wincmd p | diffthis
endif

" Mappings
" ~~~~~~~~

" back to normal mode
imap jk <Esc>

" fold/unfold
nmap <Leader><Leader> za

" exit quickly
nmap <Leader>q :q<CR>
nmap <Leader>Q :qa<CR>

" cursor moves are on displayed lines
nmap j gj
nmap k gk

" user regular expressions for searching
map / /\v
map ? ?\v

" window movement
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-h> <C-w>h
map <C-l> <C-w>l

" yank and put to/from the clipboard register
map <Leader>y "*y
map <Leader>p "*p

" toggle paste mode
map <Leader>tp :set invpaste<CR>

" toggle line numbers
nmap <Leader>tn :set relativenumber!<CR>

" make
map <Leader>m :make<CR>

" turn off search highlight until next search
map <Leader>j :nohlsearch<CR>

" select the text that I've just putted
nmap <Leader>s V']

" edit $MYVIMRC
nnoremap <Leader>rc :vsp $MYVIMRC<CR>

" faster exit
nmap <leader>q :q<CR>
nmap <leader>Q :qa<CR>

" split and focus window
nnoremap <Leader>v <C-w>v<C-w>l
nnoremap <Leader>h <C-w>s<C-w>j

" reselect visual block after indent/outdent
vnoremap < <gv
vnoremap > >gv

" make Y behave like other capitals
map Y y$

" open last/alternate buffer
noremap <Leader>a <C-^>

" easier increment/decrement
nnoremap + <C-a>
nnoremap - <C-x>

" I will ask for help when I need it
inoremap <F1> <ESC>
nnoremap <F1> <ESC>
vnoremap <F1> <ESC>

" I usually want to scroll
nmap K k

" Plugins
" ~~~~~~~

" Ack
" searches with ack the word under the cursor
map <Leader>A :Ack!<CR>

" FuzzyFinder
let g:fuf_enumeratingLimit = 10

map <Leader>b :FufBuffer<CR>
map <Leader>o :FufFile<CR>

" Gundo
let g:gundo_help = 0
let g:gundo_width = 40
let g:gundo_preview_bottom = 1
let g:gundo_right = 1
let g:gundo_close_on_revert = 1

map <Leader>u :GundoToggle<CR>

" NERDCommenter
let g:NERDCommentWholeLinesInVMode = 1

" NERDTree
let NERDTreeIgnore = ['\~$', '\.o$', '\.sw[op]$', '\.pyc$', '\.egg-info$', '\.git$', '\.aux$', '\.bbl$', '\.blg$', '\.dvi$', '.DS_Store$', '.ropeproject']
let NERDTreeShowHidden = 1
let NERDTreeShowLineNumbers = 0
let NERDTreeSortOrder = ['\/$', '\.h$', '\.c$', '*']
let NERDTreeWinSize = 30
let NERDTreeShowBookmarks=1
let NERDTreeMinimalUI = 1

" toggle NERDTree in the directory of the file that I'm editing
map <Leader>n :lcd %:h<CR>:NERDTreeToggle<CR>
map <Leader>N :NERDTree<CR>

" Syntastic
let g:syntastic_auto_jump = 1
let g:syntastic_enable_signs = 1
let g:syntastic_auto_loc_list = 1

" YankRing
let g:yankring_min_element_lenght = 2
let g:yankring_max_history = 1000
let g:yankring_history_dir = '$HOME/.vim/yankring'
let g:yankring_history_file = 'yankring_history'

" Vimwiki
map <Leader>vs :VimwikiSearch

" pep8
let g:pep8_map = "<F8>"

" pydoc
" TODO


" Fugitive
map <Leader>ga :Git add %<CR>
map <Leader>gs :Gstatus<CR>
map <Leader>gc :Gcommit<CR>

" snipMate
let g:snips_author="Alejandro Gómez <alejandroogomez@gmail.com>"

" SuperTab
let g:SuperTabDefaultCompletionType="context"

" Tabular
let g:Tabular_loaded = 1

" Taglist
let Tlist_Ctags_Cmd = "/usr/bin/ctags"
let Tlist_Use_Right_Window = 1
let Tlist_WinWidth = 35

map <Leader>T :TlistToggle<CR>

" Virtualenv
let g:virtualenv_stl_format = '<%n>'

map <Leader>w :VirtualEnvActivate

" Autocommands
" ~~~~~~~~~~~~

" dissable paste mode when leaving Insert Mode
autocmd InsertLeave * set nopaste

augroup Java
    au!

    "" Eclim
    autocmd FileType java map <Leader>i :JavaImportMissing<CR>
    autocmd FileType java map <Leader>c :JavaCorrect<CR>
    autocmd FileType java map <Leader>B :ProjectBuild<CR>
augroup END

augroup JavaScript
    au!

    autocmd FileType javascrip setlocal shiftwidth=2 tabstop=2
augroup END

augroup Python
    au!

    autocmd FileType python set omnifunc=pythoncomplete#Complete

augroup END

" automatically reload .vimrc when it's saved
autocmd BufWritePost .vimrc source %

" filetype specific settings
autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
autocmd FileType css set omnifunc=csscomplete#CompleteCSS
autocmd FileType gitcommit startinsert

" syntax highlighting
au BufRead,BufNewFile /etc/nginx/conf/* set filetype=nginx
au BufRead,BufNewFile Vagrantfile       set filetype=ruby
au BufRead,BufNewFile */.tmux.conf      set filetype=tmux
au BufRead,BufNewFile */.xmobarrc       set filetype=haskell
autocmd BufRead,BufNewFile config       set filetype=cfg
au BufRead,BufNewFile ~/.config/uzbl/*  set filetype=uzbl
au BufRead,BufNewFile */uzbl/config     set filetype=uzbl
au BufRead,BufNewFile .pentadactylrc    set filetype=vim
au BufRead,BufNewFile *.md              set filetype=markdown

" xmonad config
au BufRead xmonad\.hs set makeprg=cp\ %\ ~/.xmonad\ &&\ xmonad\ --recompile

" Makefiles depend on tabs to work
autocmd FileType make setlocal noexpandtab

" mail
autocmd FileType mail setlocal spell
autocmd FileType mail setlocal spelllang=es_ES,en_US
