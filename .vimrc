behave xterm

" 256 Color Support
"set t_Co=256
"colors molokai

set nocompatible
set relativenumber
set nofsync

call pathogen#helptags()
call pathogen#runtime_append_all_bundles()

let g:solarized_termcolors=16
let g:solarized_italic=0
let g:vimclojure#HighlightBuiltins=1
let g:vimclojure#ParenRainbow=1
let g:vimclojure#DynamicHighlighting=1


set background=dark
colors solarized

let mapleader = "\<SPACE>"

nnoremap ; :

" Source the vimrc file after saving it
nmap <silent> <leader>ev :e $MYVIMRC<CR>
if has("autocmd")
  autocmd bufwritepost .vimrc source $MYVIMRC
endif
  
" Text bubbling
nmap <C-Up> [e
nmap <C-Down> ]e
vmap <C-Up> [egv
vmap <C-Down> ]egv

" Handle window *s*plitting with leader
nmap <silent> <leader>\ <C-w>v <C-w>l
nmap <silent> <leader>- <C-w>s <C-w>j
nmap <silent> <leader>wh <C-w>h
nmap <silent> <leader>wc <C-w>c
nmap <silent> <leader>ww <C-w>w
nmap <silent> <leader>wj <C-w>j
nmap <silent> <leader>wk <C-w>k
nmap <silent> <leader>wl <C-w>l
nmap <silent> <leader>w] <C-w>]

" Tabular Plugin mappings
nmap <leader>== :Tabularize /=<CR>
vmap <leader>== :Tabularize /=<CR>
nmap <leader>=- :Tabularize /:\zs<CR>
vmap <leader>=- :Tabularize /:\zs<CR>

" Yankring
let g:yankring_max_history = 100
let g:yankring_history_file = '.yankring'

" Taglist
nmap <silent> <leader>, :TlistToggle<CR>
nmap <leader>. :tag 

" cscope
if has("cscope")
  set cscopetag
  set csto=0

  if filereadable("cscope.out")
    cs reset
    cs add cscope.out  
  endif
  
  " Find all *r*eferences to *s*ymbol under cursor
  nmap <C-\>s :cs find s <C-R>=expand("<cword>")<CR><CR>
  nmap <C-\>r :cs find s <C-R>=expand("<cword>")<CR><CR>

  " Find *g*lobal definition of token under cursor
  nmap <C-\>g :cs find g <C-R>=expand("<cword>")<CR><CR>
  
  " Find *c*alls to function under cursor
  nmap <C-\>c :cs find c <C-R>=expand("<cword>")<CR><CR>
  
  " Find all instances of *t*ext under cursor
  nmap <C-\>t :cs find t <C-R>=expand("<cword>")<CR><CR>
  
  " Find using *e*grep
  nmap <C-\>e :cs find e <C-R>=expand("<cword>")<CR><CR>
  
  " Open *f*ilename under cursor 
  nmap <C-\>f :cs find f <C-R>=expand("<cfile>")<CR><CR>
  
  " Find files that *i*nclude file under cursor
  nmap <C-\>i :cs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
  
  " Find all functions calle*d*
  nmap <C-\>d :cs find d <C-R>=expand("<cword>")<CR><CR>
endif

" Map Git
nmap <leader>gi :Git 
nmap <leader>gl :Glog<CR>
nmap <leader>gd :Gdiff<CR>
nmap <leader>gb :Gblame<CR>
nmap <leader>gc :Gcommit<CR>
nmap <leader>gs :Gstatus<CR>

" Fix searches
nnoremap / /\v
vnoremap / /\v

" Misc Mappings
nnoremap <leader><space> :noh<cr>
nnoremap <leader>cah :CalendarH<CR>
nnoremap <leader>a :Ack 
nnoremap <leader>gr :GoogleReader<CR>
nnoremap <leader>b :buffers<CR>
nnoremap <leader>pt :!phake test<CR>
nnoremap <silent> <leader>rn :set relativenumber<CR>
cmap w!! w !sudo tee % >/dev/null

" Filetype / Syntax Highlighting
syntax on
filetype on
filetype plugin on
filetype indent on
set omnifunc=syntaxcomplete#Complete

" Auto-commands
if has('autocmd')
	autocmd filetype python set expandtab
  autocmd BufRead,BufNewFile *.scss set filetype=scss
  autocmd FileType mail nmap <leader>A :w<CR>:!aspell -e -c %<CR>:e<CR>

  augroup module
    autocmd BufRead,BufNewFile *.install set filetype=php
    autocmd BufRead,BufNewFile *.module set filetype=php
    autocmd BufRead,BufNewFile *.inc set filetype=php
  augroup END
endif	

" Because paren matching makes me want to kill somebody
let loaded_matchparen = 1
set timeoutlen=3000
set ttimeout 
set ttimeoutlen=300
set pastetoggle=<F2>
set nobackup
set noswapfile
set hlsearch
set incsearch
set tabstop=2
set shiftwidth=2
set pumheight=20
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
set wildignore=*.swp,*.bak,*.orig,*.jpg,*.gif,*.png,*.swf,*.fla,*.o,.git,.svn,files/**,sites/default/files/**,backup/modules/**,no-deploy/**,sites/all/modules/ncl_endeca/docs/**,sites/all/themes/norway/html_mockups/**
set showmatch
set foldmethod=syntax
set foldenable
set foldlevel=2
set foldnestmax=10
set gfn=Inconsolata-dz:h14
set gfw=Inconsolata-dz:h14
set anti
set backspace=indent,eol,start
set history=1000
set undolevels=1000
set encoding=utf-8 nobomb
set backupskip=/tmp/*,/private/tmp/*
set completeopt=longest,menuone,preview
set laststatus=2
set statusline=%{fugitive#statusline()}\ %r\ %t%m\ %y\ Buf\ #%n\ format:\ %{&ff};\ [col\ %c:\ line\ %l\ of\ %L\ -\ %p%%]
