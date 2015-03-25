"Load Vundle plugin manager
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
"Vundle itself
Bundle 'gmarik/vundle'
"Some plugins to make HTML not horrible
Bundle 'mattn/emmet-vim'
Bundle 'gcmt/breeze.vim'
"Put closing " or ) when you write the opening one
Bundle 'tpope/vim-surround'
Bundle 'Raimondi/delimitMate'
"Current colorscheme
Bundle 'morhetz/gruvbox'
"Fuzzy finder like Sublime
Bundle 'kien/ctrlp.vim'
"Makes commenting out lines easier
Bundle 'tomtom/tcomment_vim'
"Pretty status bars - requires powerline font
Bundle 'bling/vim-airline'
"Tree view
Bundle 'scrooloose/nerdtree'
"Show changes on files in git repos
Bundle 'airblade/vim-gitgutter'
"Some more color schemes
Bundle 'flazz/vim-colorschemes'

"L33t hackers use dark themes
set t_Co=256
set background=dark

"So Vundle can do its thing
""""""""
if has('autocmd')
  filetype plugin indent on
endif
if has('syntax') && !exists('g:syntax_on')
  syntax enable
endif

" Set color schem and stop Vundle
colorscheme gruvbox
call vundle#end()

" Use :help 'option' to see the documentation for the given option.
" Lots of misc settings that make vim make more sense
" Don't remove unless you're feeling bold
set autoindent
set backspace=indent,eol,start
set complete-=i
set showmatch
set showmode
set smarttab
onoremap <silent> j gj
onoremap <silent> k gk
set nrformats-=octal
set shiftround
set ttimeout
set ttimeoutlen=50
set incsearch

" Use <C-L> to clear the highlighting of :set hlsearch.
if maparg('<C-L>', 'n') ==# ''
  nnoremap <silent> <C-L> :nohlsearch<CR><C-L>
endif

set laststatus=2
set ruler
set showcmd
set wildmenu
set autoread
set encoding=utf-8
set tabstop=2 shiftwidth=2 expandtab
set listchars=trail:▓
set list

"Because Esc is too far
inoremap jk <Esc>

"Line numbers are important
set number
"Make searching less sucky
set hlsearch
set ignorecase
set smartcase

" Shows all open buffers and their number
nnoremap <F5> :buffers<CR>:buffer<Space>

" Don't use Ex mode, use Q for formatting
map Q gq

" In many terminal emulators the mouse works just fine why not use it
if has('mouse')
  set mouse=a
endif

" do not history when leavy buffer
set hidden

"Screw ~ files
set nobackup
set nowritebackup
set noswapfile
set fileformats=unix,dos,mac

" exit insert mode
inoremap <C-c> <Esc>

set completeopt=menuone,longest,preview

"
" Plugins config
"

" NERDTree
nmap <silent> <C-n> :NERDTreeToggle<CR>

" CtrlP config
set wildignore+=*/.git/*,*/.hg/*,*/.svn/*

" vim-airline
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1

"
" Basic shortcuts definitions
" most in visual mode
"
" Find
map <C-f> /

" indend / deindent after selecting the text with (⇧ v), is a little buggy
vnoremap <Tab> >
vnoremap <S-Tab> <

" comment / decomment & normal comment behavior
vmap <C-m> gc

" Disable tComment to escape some entities
let g:tcomment#replacements_xml={}

"Function to trim trailing whitespace on save
fun! <SID>StripTrailingWhitespaces()
  let l = line(".")
  let c = col(".")
  %s/\s\+$//e
  call cursor(l,c)
endfun
autocmd BufWritePre * :call <SID>StripTrailingWhitespaces()

" lazy ':'
map ; :

"Set comma as leader button and
let mapleader = ','
nnoremap <Leader>p :set paste<CR>
nnoremap <Leader>o :set nopaste<CR>
noremap  <Leader>g :GitGutterToggle<CR>

" this machine config
if filereadable(expand("~/.vimrc.local"))
  source ~/.vimrc.local
endif
