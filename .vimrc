" pathogen
filetype off
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()
filetype plugin indent on

" not compatible with Vi
set nocompatible

" enable syntax highlighting
syntax on	
colorscheme molokai

" no annoying sounds
set visualbell

" prevent some security exploits with modelines
set modelines=0

" encoding
set encoding=utf-8

" show relative line number
set relativenumber	

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

" read changes to files outside Vim
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
"  i : current and included files
set complete=.,w,b,t,i

" autocompletion visualization
set completeopt=menuone,longest,preview

" move the cursor across empty characters
set virtualedit=all

" allow backspacing over everything on insert mode
set backspace=indent,eol,start

" hide a buffer when is abandoned
set hidden

" offset when scrolling to the top or bottom
set scrolloff=3

" number of lines to move with scroll commands
set scroll=15

" number of lines to scroll when moving the cursor off the top or bottom
set scrolljump=10

" put a $ character at the end of the text to replace
set cpoptions+=$


" redraw instead of insert/delete 
set ttyfast

" don't polute directories with swap files
set directory=~/tmp,/tmp,/var/tmp

" save undo history for each file
set undodir=~/.vim/undo
set undofile
set undolevels=1000

" do incremental searching
set incsearch	

" ignore case
set ignorecase	

" override `ignorecase` when search pattern contains upper case characters
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

" custom status line
set statusline=%<%F%y%m%{VirtualEnvStatusline()}%=%{fugitive#statusline()}[%l,%c]\ %P}

" 
" Key bindings
" ============
"

" avoid common mistakes
cmap W w
cmap Q q
cmap WQ wq
cmap Wq wq 
cmap wQ wq 

" back to normal mode
imap jj <Esc>

" don't show me the help if I don't ask for it
imap <F1> <Esc>
map <F1> <Esc>

" increase/decrease window size
" TODO

" fold/unfold
map <Space> za

" <Tab> for matching bracket pairs
map <Tab> %

" movement behaves as I like in wrapped lines
nmap j gj
nmap k gk

" search regular expressions
map / /\v

" window movement
map <C-j> <C-w>j	
map <C-k> <C-w>k	
map <C-h> <C-w>h	
map <C-l> <C-w>l	

let mapleader = ","

" yank and put to/from the clipboard register
map <Leader>y "*y
map <Leader>p "*p

" turn off search highlight untill next search
map <Leader><Space> :noh<CR>

" select the text that I've just putted
nmap <Leader>s V']

" edit $MYVIMRC
nnoremap <Leader>rc :vsp $MYVIMRC<CR>

" new vertical split with focus
nnoremap <Leader>w <C-w>v<C-w>l

" new horizontal split with focus
nnoremap <Leader>h <C-w>s<C-w>j

" save as superuser
nnoremap <Leader>sw :w !sudo tee > /dev/null<CR>


" 
" Plugins
" =======
" 

"
" Ack
" 

" searches with ack the word under the cursor
map <Leader>a :Ack!<CR>

"
" Conque-Term
"

let g:ConqueTerm_Color = 1
let g:ConqueTerm_InsertOnEnter = 1
let ConqueTerm_ReadUnfocused = 1

"
" Gundo
"

let g:gundo_help = 0
let g:gundo_width = 30
let g:gundo_preview_bottom = 1
let g:gundo_right = 1
let g:gundo_close_on_revert = 1

"
" Makegreen
"

" TODO

" test Django app
map <leader>dt :set makeprg=python\ manage.py\ test\|:call MakeGreen()<CR>

"
" MiniBufExplorer
"

" TODO

" 
" NERDCommenter
"

let g:NERDCommentWholeLinesInVMode = 1


"
" NERDTree
"

map <Leader>n :NERDTreeToggle<CR>

let NERDTreeIgnore = ['\~$', '\.o$', '\.sw[op]$', '\.pyc$', '\.git$', '.DS_Store$']
let NERDTreeShowHidden = 1
let NERDTreeShowLineNumbers = 1
let NERDTreeSortOrder = ['\/$', '\.h$', '\.c$', '*']
let NERDTreeWinSize = 25
let NERDTreeMinimalUI = 1

"
" Scratch
"

" TODO

"
" Syntastic
"

let g:syntastic_auto_jump = 1

" 
" TaskList
" 

" TODO

"
" YankRing
"

" don't store one character yanks in the YankRing
let yankring_min_element_lenght = 2

"
" Repeat
"

" TODO

"
" Surround
"

" TODO

"
" Fugitive
"
map <Leader>ga :Git add %<CR>
map <Leader>gs :Gstatus<CR>
map <Leader>gc :Gcommit<CR>
map <Leader>gl :Glog<CR>

"
" pep8
"

let g:pep8_map ='<leader>8'

"
" pydoc
"

" TODO

"
" Rope
"

map <Leader>j :RopeGotoDefinition<CR>
map <Leader>r :RopeRename<CR>

"
" SuperTab
"

let g:SuperTabDefaultCompletionType="context"

"
" Tabular
"

let g:Tabular_loaded = 1

"
" Virtualenv
"

let g:virtualenv_stl_format = '<%n>'
