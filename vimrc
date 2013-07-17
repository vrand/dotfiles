set nocompatible

" <Space> is easier to type than "\"
let mapleader = " "

" Plugins
" ~~~~~~~

if has('vim_starting')
    set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#rc(expand('~/.vim/bundle/'))

" shortcut for installing bundles
map <Leader>I :NeoBundleInstall<CR>

" Let NeoBundle manage NeoBundle
NeoBundleFetch 'Shougo/neobundle.vim'

" Unite
NeoBundle 'Shougo/unite.vim'
NeoBundle 'ujihisa/unite-colorscheme'
NeoBundle 'tsukkee/unite-help'
NeoBundle 'h1mesuke/unite-outline'

" External utilities
""NeoBundle 'rking/ag.vim'
NeoBundle 'tpope/vim-eunuch'
NeoBundle 'Shougo/vimproc.vim'

" Windows
"NeoBundle 'jimsei/winresizer'

" Colors
NeoBundle 'tpope/vim-vividchalk'
NeoBundle 'tomasr/molokai'
NeoBundle 'sjl/badwolf'
NeoBundle 'vim-scripts/Colour-Sampler-Pack'
NeoBundle 'vim-scripts/louver.vim'
NeoBundle 'noahfrederick/Hemisu'
NeoBundle 'vim-scripts/summerfruit256.vim'
NeoBundle 'Rykka/colorv.vim'
NeoBundle 'trapd00r/neverland-vim-theme'

" Version Control
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'airblade/vim-gitgutter'

" Text editing
"NeoBundle 'tpope/vim-surround'
"NeoBundle 'tpope/vim-repeat'
NeoBundle 'Shougo/neocomplcache'
""NeoBundle 'SuperTab-continued.'

" Comments
NeoBundle 'scrooloose/nerdcommenter'

" Navigation
NeoBundle 'scrooloose/nerdtree'
"
" Linters
"NeoBundle 'scrooloose/syntastic'

" Syntax highlighting
"NeoBundle 'kchmck/vim-coffee-script'
""NeoBundle 'groenewege/vim-less'
"NeoBundle 'skammer/vim-css-color'
""NeoBundle 'hail2u/vim-css3-syntax'
"NeoBundle 'saltstack/salt-vim'
"NeoBundle 'nginx.vim'

" Python
NeoBundle 'klen/python-mode'
NeoBundle 'jmcantrell/vim-virtualenv'

" Haskell
NeoBundle 'eagletmt/ghcmod-vim'
NeoBundle 'ujihisa/neco-ghc'

" Clojure
"NeoBundle 'paredit.vim'

" HTML
"NeoBundle 'othree/html5.vim'

" Misc
NeoBundle 'vimwiki'
"NeoBundle 'scratch'
NeoBundle 'ZoomWin'

" Tasks
"NeoBundle 'LStinson/TagmaTasks'

filetype plugin indent on

" General
" ~~~~~~~

" Vim settings rather than Vi settings
set nocompatible

set nobackup
set noswapfile

" save undo history for each file
set undodir=~/.vim/undo
set undofile
set undolevels=1000

" encoding
set encoding=utf-8

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
set wildignore+=.coverage,coverage.xml,nosetests.xml,.noseids

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

" Highliht extra whitespace when leaving insert mode
autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red
autocmd InsertLeave * match ExtraWhitespace /\s\+$/

function! DarkColorScheme()
    set bg=dark
    colorscheme hemisu
endfunc

function! LightColorScheme()
    set bg=light
    colorscheme hemisu
endfunc

function! TransparentColorScheme()
    set bg=dark
    colorscheme xemacs
endfunc

syntax enable

call DarkColorScheme()

" Swap light/dark/transparent colorschemes
nmap <Leader>lc :call LightColorScheme()<CR>
nmap <Leader>dc :call DarkColorScheme()<CR>
nmap <Leader>nc :call TransparentColorScheme()<CR>

" UI
" ~~

" last window always has a status line
set laststatus=2

set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P

" show information about the cursor coords and relative position
set ruler

" highlight current line and column
set cursorline
set cursorcolumn

" wrap long lines
set wrap
set textwidth=79
"set formatoptions=tcqn

" visual indicator in 80-th column
set colorcolumn=80

" Editing
" ~~~~~~~

" show invisible chars (*t*oggle *l*ist)
nmap <leader>tl :set list!<CR>
set list
set listchars=tab:▸\ ,eol:¬,trail:·,extends:↷,precedes:↶
"
function! Spanish()
    set spell
    set spelllang=es
endfunc

function! English()
    set spell
    set spelllang=en
endfunc

" *t*oggle *s*pelling / switch language
nmap <leader>ts :set spell!<CR>
set spelllang=en
"
nmap <leader>ss :call Spanish()<CR>
nmap <leader>se :call English()<CR>

" show relative line number
set relativenumber
"set number
set numberwidth=3

" read changes to files outside Vim
set autoread
"
" 4 spaces instead of <Tab>
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab

" indentation behaviour
set autoindent
set copyindent
"
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

" put a '$' at the end of the text to change when using change command
set cpoptions+=$

" Search
" ~~~~~~

" incremental searching
set incsearch
"
" wrap the bottom and top of the file while searching
set wrapscan
"
" case insensitive search unless pattern contains upper case characters
set ignorecase
set smartcase
"
" highlight all matches with the search pattern
set hlsearch
"
" global flag automatically on with the substitute command
set gdefault
"
" Commands
" ~~~~~~~~

" remove trailing whitespaces in the whole file
command! -nargs=* StripTrailingWhitespace :%s/\s\+$//g
map <Leader>st :StripTrailingWhitespace<CR>''
"
"" remove those nasty ^M from a file
command! -nargs=* StripCarriageReturn :%s/\r$//g
map <Leader>sc :StripCarriageReturn<CR>''

" avoid common mistakes
cmap Q q
cmap WQ wq
cmap Wq wq
cmap wQ wq
cmap w1 w!

" Mappings
" ~~~~~~~~

" back to normal mode
imap jk <Esc>

" fold/unfold
nmap <Leader><Leader> za

" exit quickly
nmap <Leader>q :q!<CR>
nmap <Leader>Q :qa!<CR>

" cursor moves are on displayed lines
nmap j gj
nmap k gk

" Very magin search
map / /\v

" window movement
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-h> <C-w>h
map <C-l> <C-w>l
"
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
"
" edit $MYVIMRC
nnoremap <Leader>rc :e $MYVIMRC<CR>

" populate file with cURL
command! -nargs=1 CurlAndPut :.!curl <args> 2> /dev/null
nnoremap <Leader>cp :CurlAndPut<Space>

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

" save
nmap <Leader>w :silent! update<CR>

" easier increment/decrement
nnoremap + <C-a>
nnoremap - <C-x>

" I will ask for help when I need it
inoremap <F1> <ESC>
nnoremap <F1> <ESC>
vnoremap <F1> <ESC>

" I usually want to scroll
nmap K k


" Plugin configuration
" ~~~~~~~~~~~~~~~~~~~~

" Unite
call unite#filters#matcher_default#use(['matcher_fuzzy'])

let g:unite_source_history_yank_enable = 1
let g:unite_prompt = '» '
let g:unite_enable_start_insert = 1
let g:unite_source_history_yank_limit = 10000
let g:unite_split_rule = 'botright'
let g:unite_source_history_yank_file = "/home/dialelo/.vim/yanks"
" FIXME
"let g:unite_source_grep_command = 'ag'
"let g:unite_source_grep_default_opts = '--no-heading --no-color -a'
"let g:unite_source_grep_recursive_opt = ''


"nnoremap <Leader>/ :Unite grep<CR>
nnoremap <C-o> :Unite -no-split file_rec<CR>
nnoremap <C-b> :Unite -no-split buffer<CR>
nnoremap <C-y> :Unite history/yank<CR>
nnoremap <Leader>cs  :Unite -auto-preview colorscheme<CR>
nnoremap <Leader>k  :Unite -no-split mapping<CR>
nnoremap <Leader>l :Unite -no-split line<CR>
nnoremap <Leader>t :Unite outline<CR>


" Custom mappings for the unite buffer
autocmd FileType unite call s:unite_settings()
function! s:unite_settings()
  " Enable navigation with control-j and control-k in insert mode
  imap <buffer> <C-j>   <Plug>(unite_select_next_line)
  imap <buffer> <C-k>   <Plug>(unite_select_previous_line)
endfunction

"" vim-eunuch
map <Leader>sw :SudoWrite<CR>
map <Leader>f  :Find<Space>
map <Leader>x  :Chmod +x<CR>

" NERDCommenter
let g:NERDCommentWholeLinesInVMode = 1

" NERDTree
let NERDTreeIgnore = ['\~$', '\.tox$', 'dist', '\.o$', '\.sw[op]$', '\.pyc$', '\.egg-info$', '\.git$', '\.aux$', '\.bbl$', '\.blg$', '\.dvi$', '.DS_Store$', '.ropeproject']
let NERDTreeShowHidden = 1
let NERDTreeShowLineNumbers = 0
let NERDTreeSortOrder = ['\/$', '\.h$', '\.c$', '*']
let NERDTreeWinSize = 30
let NERDTreeShowBookmarks=1
let NERDTreeMinimalUI = 1
"
" toggle NERDTree in the directory of the file that I'm editing
map <Leader>n :NERDTreeToggle<CR>
map <Leader>N :NERDTree<CR>
"
"" Syntastic
"let g:syntastic_auto_jump = 1
"let g:syntastic_enable_signs = 1
"let g:syntastic_auto_loc_list = 2
"let g:syntastic_quiet_warnings = 1

"" python-mode
"let g:pymode_lint_ignore = "E501"
"let g:pymode_lint_cwindow = 0
"let g:pymode_breakpoint_key = '<Leader>B'
"let g:pymode_options = 0
"
"" pydoc
"" TODO
"
"" RopeVim
"map <Leader>xm :RopeExtractMethod<CR>
"map <Leader>g :RopeGotoDefinition<CR>
"
"" Fugitive
map <Leader>gw :Gwrite<CR>
map <Leader>gA :Git add -u<CR>
map <Leader>gr :Gread<CR>
map <Leader>gd :Gdiff<CR>
map <Leader>gR :Gremove<CR>
map <Leader>gs :Gstatus<CR>
map <Leader>gc :Gcommit<CR>
map <Leader>gb :Gblame<CR>
map <Leader>gp :Git pull<CR>
map <Leader>gP :Git push<CR>

map <Leader>du :diffupdate<CR>

" Virtualenv
let g:virtualenv_stl_format = '<%n>'

map <Leader>e :VirtualEnvActivate<Space>

" Neocomplcache
let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_enable_smart_case = 1
let g:neocomplcache_enable_camel_case_completion = 1
let g:neocomplcache_enable_underbar_completion = 1

" neco-ghc
let g:necoghc_enable_detailed_browse = 1

" Autocommands
" ~~~~~~~~~~~~

" dissable paste mode when leaving Insert Mode
autocmd InsertLeave * set nopaste

" save when leaving Insert Mode
"autocmd InsertLeave * :silent! update

" automatically reload .vimrc when it's saved
"autocmd BufWritePost vimrc source %

au BufRead xmonad.hs set makeprg=xmonad\ --recompile

" filetype specific settings

"" syntax highlighting
au BufRead,BufNewFile nginx.conf set filetype=nginx


" Vagrantfile
au BufRead,BufNewFile Vagrantfile set filetype=ruby

" tmux configuration
au BufRead,BufNewFile tmux.conf set filetype=tmux

" turses config files
au BufRead,BufNewFile config   set filetype=cfg
au BufRead,BufNewFile token    set filetype=cfg
au BufRead,BufNewFile sessions set filetype=cfg

" uzbl config
au BufRead,BufNewFile ~/config/uzbl/* set filetype=uzbl
au BufRead,BufNewFile */uzbl/config   set filetype=uzbl

" markdown
au BufRead,BufNewFile *.md set filetype=markdown

" Django templates
au BufRead,BufNewFile */templates/*.html setlocal filetype=htmldjango

" Save and load the state of the document (folding and cursor line)
au BufWinLeave * mkview
au BufWinEnter * silent! loadview

autocmd BufWritePost *.hs :GhcModCheckAndLintAsync
