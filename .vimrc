" ------------ Priority Settings ---------
behave xterm

set nocompatible
set nofsync

" Filetype / Syntax Highlighting
syntax on
filetype on
filetype plugin on
filetype indent on
set omnifunc=syntaxcomplete#Complete

" Pathogen for loading plugins
call pathogen#helptags()
call pathogen#runtime_append_all_bundles()

" ------------ Plugin Settings -----------

" Solarized settings
let g:solarized_termcolors=16
let g:solarized_italic=0
set background=dark
colors solarized

" Mapping
let mapleader = "\<SPACE>"
nnoremap ; :

" Tab matches parens...
nnoremap <tab> %
vnoremap <tab> %

" Gist
"let g:gist_clip_command = 'pbcopy'
"let g:gist_detect_filetype = 1
"let g:gist_open_browser_after_post = 1

" Ctrl-P
nnoremap <leader>t :CtrlP<cr>
nnoremap <leader>b :CtrlPBuffer<cr>

let g:ctrlp_working_path_mode=1
let g:ctrlp_user_command = 'find %s -type f'
let g:ctrlp_use_caching = 1
let g:ctrlp_clear_cache_on_exit = 0
let g:ctrlp_cache_dir = $HOME.'/.cache/ctrlp'
let g:ctrlp_max_files = 20000
let g:ctrlp_lazy_update = 1

set wildignore+=.git,.svn
set wildignore+=*.class,*.jar,*.swp,*.bak,*.orig
set wildignore+=*.jpg,*.gif,*.png,*.swf,*.fla,*.o
set wildignore+=files/**,sites/default/files/**
set wildignore+=backup/modules/**,no-deploy/**
set wildignore+=sites/all/modules/ncl_endeca/docs/**
set wildignore+=classes

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

" Globals for VimClojure
let g:vimclojure#HighlightBuiltins=1
let g:vimclojure#ParenRainbow=1
let g:vimclojure#DynamicHighlighting=1

" Map Git (requires tpope's vim-fugitive)
nmap <leader>gi :Git 
nmap <leader>gl :Glog<CR>
nmap <leader>gd :Gdiff<CR>
nmap <leader>gb :Gblame<CR>
nmap <leader>gc :Gcommit<CR>
nmap <leader>gs :Gstatus<CR>

" vimclojure
let vimclojure#WantNailgun = 1 
let vimclojure#SplitPos = "right"
let tlist_clojure_settings = 'lisp;f:function'
let tlist_exit_OnlyWindow = 1

" ------------ Custom Mappings -----------

" Edit .vimrc
nmap <silent> <leader>ev :e $MYVIMRC<CR>

" Learn to use Vim
map <Left> <Nop>
map <Right> <Nop>
map <Up> <Nop>
map <Down> <Nop>
  
" Text bubbling (requires tpope's vim-unimpaired/vim-repeat)
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

" Fix searches
nnoremap / /\v
vnoremap / /\v

" Misc Mappings
nnoremap <silent><leader>sp :setlocal spell spelllang=en_us<cr>
cmap w!! w !sudo tee % >/dev/null

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

" ------------ Custom Functions ----------

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
function! OpenPhpFunction (keyword)
  let proc_keyword = substitute(a:keyword , '_', '-', 'g')
  exe 'silent r!open http://php.net/'.proc_keyword
endfunction

function! OpenPhpPane (keyword)
  exe 'vnew'
  exe "set buftype=nofile"
  exe "setlocal noswapfile"
  exe 'silent r!php-man '.a:keyword
  exe 'go 1'
endfunction

function! OpenDrupalFunction (keyword)
  let proc_keyword = substitute(a:keyword , '#', '-', 'g')
  exe 'silent r!open http://api.drupal.org/api/search/6/'.proc_keyword
endfunction

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

" ------------ Auto-commands -------------
if has('autocmd')
  " Help vim recognize file extensions
  autocmd BufRead,BufNewFile *.scss set filetype=scss
  autocmd BufRead,BufNewFile *.cljs set filetype=clojure
  autocmd BufRead,BufNewFile *.ddl set filetype=sql
  autocmd BufRead,BufNewFile *.htdata set filetype=html

  " Language-specific settings
  autocmd FileType ruby,haml,eruby,yaml,html,javascript,sass,cucumber set ai sw=2 sts=2 et
  autocmd FileType python set sw=4 sts=4 et

  augroup php
    autocmd FileType php map <leader>k :call OpenDrupalFunction('<C-r><C-w>')<CR>  
    autocmd FileType php map K :call OpenPhpPane('<C-r><C-w>')<CR>  
    autocmd BufRead,BufNewFile *.install set filetype=php
    autocmd BufRead,BufNewFile *.module set filetype=php
    autocmd BufRead,BufNewFile *.inc set filetype=php
  augroup END

  autocmd bufwritepost .vimrc source $MYVIMRC
  autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif
endif	

" ------------ Vim Settings --------------
let loaded_matchparen = 1 " Because paren matching makes me want to kill somebody
set switchbuf=useopen
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
set relativenumber 
set numberwidth=5
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
set showmatch
set foldmethod=syntax
set foldenable
set foldlevel=2
set foldnestmax=10
set gfn=Menlo:h14
set gfw=Menlo:h14
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
