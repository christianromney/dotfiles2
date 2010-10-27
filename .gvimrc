set nocompatible
set relativenumber

call pathogen#helptags()
call pathogen#runtime_append_all_bundles()

let mapleader = "\<SPACE>"

nnoremap ; :
nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>
nmap <leader>b :buffers<CR>
nmap <leader>pt :!phake test<CR>
cmap w!! w !sudo tee % >/dev/null

nnoremap / /\v
vnoremap / /\v
nnoremap <leader><space> :noh<cr>
nnoremap <leader>a :Ack 

behave xterm
filetype on
filetype plugin on
filetype indent on

colors molokai

if has('autocmd')
	autocmd filetype python set expandtab
endif	

set pastetoggle=<F2>
set nobackup
set noswapfile
set hlsearch
set incsearch
set tabstop=2
set shiftwidth=2
set shiftround
set autoindent
set copyindent
set number 
set ruler
set nowrap
set title
set ignorecase
set smartcase
set smarttab
set showcmd
set showmode
set expandtab
set hidden
set shortmess=atI
set visualbell
set noerrorbells
set scrolloff=3
set wildmenu
set wildmode=list:longest
set wildignore=*.swp,*.bak
set showmatch
set foldmethod=syntax
set gfn=Inconsolata-dz:h14
set gfw=Inconsolata-dz:h14
set anti
set backspace=indent,eol,start
set history=1000
set undolevels=1000
set enc=utf-8
set encoding=utf8 nobomb
set backupskip=/tmp/*,/private/tmp/*
