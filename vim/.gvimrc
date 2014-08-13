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

" Prevent Vim from clobbering the scrollback buffer. See
" http://www.shallowsky.com/linux/noaltscreen.html
set t_ti= t_te=

" Key Mappings
" Space is the only leader big enough for my tastes
let mapleader = "\<SPACE>"
nnoremap ; :

" Tab matches parens...
nnoremap <tab> %
vnoremap <tab> %

" Two j's in a row
inoremap jj <ESC>

" Select pasted item
nnoremap <leader>v V`]

" It's the 21st Century.
noremap j gj
noremap k gk

" Learn to use Vim
map <Left> <Nop>
map <Right> <Nop>
map <Up> <Nop>
map <Down> <Nop>

" Keep search matches in the middle of the window and pulse the line when moving
" to them.
nnoremap n nzzzv:call PulseCursorLine()<cr>
nnoremap N Nzzzv:call PulseCursorLine()<cr>

" Ctrl-P (command-t replacement)
nnoremap <leader>t :CtrlP<cr>

" Changes cwd to nearest ancestor containing a .git folder
let g:ctrlp_working_path_mode=2

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
nmap <leader>tt :Tabularize /=><CR>
vmap <leader>tt :Tabularize /=><CR>
nmap <leader>te :Tabularize /=<CR>
vmap <leader>te :Tabularize /=<CR>
nmap <leader>tc :Tabularize /:\zs<CR>
vmap <leader>tc :Tabularize /:\zs<CR>

" Yankring
let g:yankring_max_history = 100
let g:yankring_history_file = '.yankring'
"
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
cmap w!! w !sudo tee % >/dev/null

function! InsertTabWrapper()
  let col = col('.') - 1
  if !col || getline('.')[col - 1] !~ '\k'
    return "\<tab>"
  else
    return "\<c-p>"
  endif
endfunction
 
inoremap <tab> <c-r>=InsertTabWrapper()<cr>
inoremap <s-tab> <c-n>                                                                                                                                                
 
" Clear the search buffer when hitting return
function! MapCR()
  nnoremap <cr> :nohlsearch<cr>
endfunction
call MapCR()

set omnifunc=syntaxcomplete#Complete

" Auto-commands
if has('autocmd')
  autocmd FocusLost * :wa
  autocmd BufRead,BufNewFile *.scss set filetype=scss
  autocmd FileType php map K :call OpenPhpFunction('<C-r><C-w>')<CR>
  autocmd FileType ruby,haml,eruby,yaml,html,javascript,sass,cucumber set ai sw=2 sts=2 et                                                                            
  autocmd FileType python set sw=4 sts=4 et

  augroup module
    autocmd BufRead,BufNewFile *.install set filetype=php
    autocmd BufRead,BufNewFile *.module set filetype=php
    autocmd BufRead,BufNewFile *.inc set filetype=php
  augroup END

  augroup ft_java
      au!
      au FileType java setlocal foldmethod=marker
      au FileType java setlocal foldmarker={,}
  augroup END

  augroup ft_cljs
    au BufRead,BufNewFile *.cljs set filetype=clojure
  augroup END

  augroup ft_javascript
      au!
      au FileType javascript setlocal foldmethod=marker
      au FileType javascript setlocal foldmarker={,}
  augroup END

  autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif
endif	

" Because paren matching makes me want to kill somebody
let loaded_matchparen = 1
set switchbuf=useopen
set timeoutlen=3000
set ttimeout 
set ttimeoutlen=300
set modelines=0
set autoread
set ttyfast
set relativenumber
set numberwidth=5
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
set foldenable
set foldlevel=2
set foldnestmax=10
set gfn=Menlo:h18
set gfw=Menlo:h18
set anti
set backspace=indent,eol,start
set history=1000
set undolevels=1000
set encoding=utf-8 nobomb
set backupskip=/tmp/*,/private/tmp/*
set completeopt=longest,menuone,preview
set laststatus=2
set statusline=%{fugitive#statusline()}\ %r\ %t%m\ %y\ Buf\ #%n\ format:\ %{&ff};\ [col\ %c:\ line\ %l\ of\ %L\ -\ %p%%]
set cursorline

" RENAME CURRENT FILE
function! RenameFile()
    let old_name = expand('%')
    let new_name = input('New file name: ', expand('%'), 'file')
    if new_name != '' && new_name != old_name
        exec ':saveas ' . new_name
        exec ':silent !rm ' . old_name
        redraw!
    endif
endfunction
map <leader>n :call RenameFile()<cr>

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

" Pulse Cusor Line (Steve Losh) {{{
function! PulseCursorLine()
    let current_window = winnr()

    windo set nocursorline
    execute current_window . 'wincmd w'

    setlocal cursorline

    redir => old_hi
        silent execute 'hi CursorLine'
    redir END
    let old_hi = split(old_hi, '\n')[0]
    let old_hi = substitute(old_hi, 'xxx', '', '')

    hi CursorLine guibg=#2a2a2a ctermbg=233
    redraw
    sleep 20m
  
    hi CursorLine guibg=#333333 ctermbg=235
    redraw
    sleep 20m

    hi CursorLine guibg=#3a3a3a ctermbg=237
    redraw
    sleep 20m

    hi CursorLine guibg=#444444 ctermbg=239
    redraw
    sleep 20m

    hi CursorLine guibg=#3a3a3a ctermbg=237
    redraw
    sleep 20m

    hi CursorLine guibg=#333333 ctermbg=235
    redraw
    sleep 20m

    hi CursorLine guibg=#2a2a2a ctermbg=233
    redraw
    sleep 20m

    execute 'hi ' . old_hi

    windo set cursorline
    execute current_window . 'wincmd w'
endfunction

" }}}
