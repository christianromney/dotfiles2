behave xterm
set nocompatible
set relativenumber

colors molokai

call pathogen#helptags()
call pathogen#runtime_append_all_bundles()

let mapleader = "\<SPACE>"

nnoremap ; :
nmap <silent> <leader>ev :e $MYVIMRC<CR>

" Handle window *s*plitting with leader
nmap <silent> <leader>ws <C-w>s
nmap <silent> <leader>wv <C-w>v
nmap <silent> <leader>wc <C-w>c
nmap <silent> <leader>ww <C-w>w
nmap <silent> <leader>wh <C-w>h
nmap <silent> <leader>wj <C-w>j
nmap <silent> <leader>wk <C-w>k
nmap <silent> <leader>wl <C-w>l

" Taglist
nmap <silent> <leader>, :TlistToggle<CR>

" Map Git
nmap <leader>gi :Git 
nmap <leader>gt :Gist
nmap <leader>gp :Gist -p

" Fix searches
nnoremap / /\v
vnoremap / /\v

" Misc Mappings
nnoremap <leader><space> :noh<cr>
nnoremap <leader>ch :CalendarH<CR>
nnoremap <leader>a :Ack 
nnoremap <leader>gr :GoogleReader<CR>
nnoremap <leader>b :buffers<CR>
nnoremap <leader>pt :!phake test<CR>
nnoremap <silent> <leader>rn :set relativenumber<CR>
cmap w!! w !sudo tee % >/dev/null


" Omni-Completion Mappings
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
inoremap <expr> <C-n> pumvisible() ? '<C-n>' :
  \ '<C-n><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'
inoremap <expr> <M-,> pumvisible() ? '<C-n>' :
  \ '<C-x><C-o><C-n><C-p><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'


filetype on
filetype plugin on
filetype indent on

" Auto-commands
if has('autocmd')
	autocmd filetype python set expandtab
  autocmd BufRead,BufNewFile *.scss set filetype=scss
  augroup module
    autocmd BufRead,BufNewFile *.install set filetype=php
    autocmd BufRead,BufNewFile *.module set filetype=php
  augroup END
  augroup inc
    autocmd BufRead,BufNewFile *.inc set filetype=php
  augroup END
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
set completeopt=longest,menuone
