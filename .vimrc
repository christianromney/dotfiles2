behave xterm

" 256 Color Support
set t_Co=256
set nocompatible
set relativenumber
set nofsync
colors molokai

call pathogen#helptags()
call pathogen#runtime_append_all_bundles()

let mapleader = "\<SPACE>"

nnoremap ; :
nmap <silent> <leader>ev :e $MYVIMRC<CR>

" Save file
nmap <leader>s :w<CR>
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

" Conque Shell
if has("python")
  let g:ConqueTerm_Color = 1
  let g:ConqueTerm_TERM = 'xterm'
  let g:ConqueTerm_ReadUnfocused = 1

  nmap <silent> <leader>ct :ConqueTerm login -fp christian<CR>
  nmap <silent> <leader>ch :ConqueTermSplit login -fp christian<CR>
  nmap <silent> <leader>cv :ConqueTermVSplit login -fp christian<CR>
endif

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
  
  nmap <C-\>s :cs find s <C-R>=expand("<cword>")<CR><CR>
  nmap <C-\>g :cs find g <C-R>=expand("<cword>")<CR><CR>
  nmap <C-\>c :cs find c <C-R>=expand("<cword>")<CR><CR>
  nmap <C-\>t :cs find t <C-R>=expand("<cword>")<CR><CR>
  nmap <C-\>e :cs find e <C-R>=expand("<cword>")<CR><CR>
  nmap <C-\>f :cs find f <C-R>=expand("<cfile>")<CR><CR>
  nmap <C-\>i :cs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
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
nnoremap <silent> <leader>rn :set relativenumber<CR>
cmap w!! w !sudo tee % >/dev/null

" Filetype / Syntax Highlighting
syntax on
filetype on
filetype plugin on
filetype indent on
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
set wildignore=*.swp,*.bak
set showmatch
set foldmethod=syntax
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
