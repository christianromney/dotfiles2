behave xterm

set nocompatible
set nofsync

call pathogen#helptags()
filetype off
call pathogen#runtime_append_all_bundles()
filetype plugin indent on

" Globals for VimClojure
let g:vimclojure#HighlightBuiltins=1
let g:vimclojure#ParenRainbow=1
let g:vimclojure#DynamicHighlighting=1

" Solarized settings
let g:solarized_termcolors=16
let g:solarized_italic=0
set background=dark
colors solarized

" Key Mappings
" Space is the only leader big enough for my tastes
let mapleader = "\<SPACE>"
nnoremap ; :

" Tab matches parens...
nnoremap <tab> %
vnoremap <tab> %

" Four semi-colons go back to normal mode
inoremap ;;;; <ESC>

" Command-T
let g:CommandTMaxHeight = 15
set wildignore+=.git,.svn
set wildignore+=*.class,*.jar,*.swp,*.bak,*.orig
set wildignore+=*.jpg,*.gif,*.png,*.swf,*.fla,*.o
set wildignore+=files/**,sites/default/files/**
set wildignore+=backup/modules/**,no-deploy/**
set wildignore+=sites/all/modules/ncl_endeca/docs/**

" Vim Settings
nmap <silent> <leader>ev :e $MYGVIMRC<CR>
if has("autocmd")
  autocmd bufwritepost .gvimrc source $MYGVIMRC
endif

" Text bubbling
nmap <C-Up> [e
nmap <C-Down> ]e
vmap <C-Up> [egv
vmap <C-Down> ]egv

" Handle window *s*plitting with leader
nmap <silent> <leader>ws <C-w>s
nmap <silent> <leader>wv <C-w>v
nmap <silent> <leader>wc <C-w>c
nmap <silent> <leader>ww <C-w>w
nmap <silent> <leader>wh <C-w>h
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
nmap <leader>gt :Gist
nmap <leader>gp :Gist -p
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
cmap w!! w !sudo tee % >/dev/null

set omnifunc=syntaxcomplete#Complete

" Custom Functions
function! OpenPhpFunction (keyword)
  let proc_keyword = substitute(a:keyword , '_', '-', 'g')
  try
    exe 'pedit'
  catch /.*/
  endtry
  exe 'wincmd P'
  exe 'enew'
  exe "set buftype=nofile"
  exe "setlocal noswapfile"
  exe 'silent r!links -dump http://php.net/'.proc_keyword
  exe 'norm gg'
  exe 'call search("____________________________________")'
  exe 'norm dgg'
  exe 'call search("User Contributed Notes")'
  exe 'norm dGgg'
endfunction

" Auto-commands
if has('autocmd')
  autocmd FocusLost * :wa
	autocmd filetype python set expandtab
  autocmd BufRead,BufNewFile *.scss set filetype=scss
  autocmd FileType php map K :call OpenPhpFunction('<C-r><C-w>')<CR>
  "autocmd filetype php set keywordprg=":help"
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
set modelines=0
set ttyfast
set relativenumber
set pastetoggle=<F2>
set cursorline
set nobackup
set noswapfile
set hlsearch
set incsearch
set showmatch
set tabstop=2
set shiftwidth=2
set pumheight=20
set shiftround
set autoindent
set copyindent
set number 
set ruler
set colorcolumn=85
set formatoptions=qrn1
set nowrap
set title
set ignorecase
set gdefault
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
set foldmethod=syntax
set gfn=Inconsolata-dz:h18
set gfw=Inconsolata-dz:h18
set anti
set backspace=indent,eol,start
set history=1000
set undolevels=1000
set encoding=utf-8 nobomb
set backupskip=/tmp/*,/private/tmp/*
set completeopt=longest,menuone,preview
set laststatus=2
set statusline=%{fugitive#statusline()}\ %r\ %t%m\ %y\ Buf\ #%n\ format:\ %{&ff};\ [col\ %c:\ line\ %l\ of\ %L\ -\ %p%%]
