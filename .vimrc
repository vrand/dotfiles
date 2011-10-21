" pathogen
filetype off
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()
filetype plugin indent on

" not compatible with Vi
set nocompatible

" prevent some security exploits with modelines
set modelines=0

" encoding
set encoding=utf-8

" show relative line number
set relativenumber	

" enable syntax highlighting
syntax on	

" elegant colorscheme
colorscheme wombat256

" command line height
set cmdheight=1

" show current mode at the bottom
set showmode

" display info about current command at the bottom
set showcmd

" enable status line at the bottom
set laststatus=2

" information about the document at the bottom
set ruler

" read changes to file outside Vim
set autoread	

" highlight current line
set cursorline

" 4 spaces instead of <Tab>
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab

" smart indenting for C-like languages
set smartindent

" folding 
set foldmethod=indent

" enable autocompletion menu
set wildmenu	
set wildignore+=*.o,*.class,.git,*.pyc
set wildmode=list:full

" tells VIM where to search for autocompletion
"  . : current file
"  w : files in other windows
"  b : files in loaded buffers, not in a window
"  t : the `tags` file
set complete=.,w,b,t

" move the cursor across empty characters
set virtualedit=all

" allow backspacing over everything on insert mode
set backspace=indent,eol,start

" hide a buffer when is abandoned
set hidden

" offset when scrolling to the top or bottom
set scrolloff=3

" number of lines to move with scroll commands
" set scroll=10

" number of lines to scroll when moving the cursor
" off the top or bottom
" set scrolljump=10

" put a $ character at the end of the text to replace
set cpoptions+=$

" no annoying sounds
set visualbell

" redraw instead of insert/delete 
set ttyfast

" save undo history for each file
set undodir=~/.vim/undo
set undofile
set undolevels=1000

" do incremental searching
set incsearch	

" ignore case
set ignorecase	

" override `ignorecase` when search 
" pattern contains upper case characters
set smartcase	

" highlight all matches with the search pattern
set hlsearch

" wrap long lines
set wrap

" wrap the bottom and top of the file while searching
set wrapscan

" color a column
set colorcolumn=85

" g flag automatically on with the substitute command
set gdefault

" ================================================== "

let mapleader = ","

" movement behaves as I like in wrapped lines
nnoremap j gj
nnoremap k gk

" type commands easier
nnoremap ç :

" search regular expressions
nnoremap / /\v
vnoremap / /\v

" turn off search highlight untill next search
map <Leader><Space> :noh<CR>

" window movement
map <C-j> <C-w>j	
map <C-k> <C-w>k	
map <C-h> <C-w>h	
map <C-l> <C-w>l	

" focus on current window
map <C-o> <C-w>o

" select the text that I've just putted
nnoremap <Leader>s V']

" edit $MYVIMRC
nnoremap <Leader>rc :vsp $MYVIMRC<CR>

" new vertical split with focus
nnoremap <Leader>w <C-w>v<C-w>l

" new horizontal split with focus
nnoremap <Leader>h <C-w>s<C-w>j

" save as superuser
nnoremap <Leader>sw :w !sudo tee > /dev/null<CR>

" fold/unfold
nnoremap <Space> za

" back to normal mode
inoremap jj <Esc>

" <Tab> for matching bracket pairs
map <Tab> %

" NERDTree
map <Leader>n :NERDTreeToggle<CR>

" Conque-Term
let g:ConqueTerm_Color = 1
let g:ConqueTerm_InsertOnEnter = 1
let g:ConqueTerm_ReadUnfocused = 1
