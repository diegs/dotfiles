" For plugin loading.
set nocompatible
filetype off
syntax off

" Set leader before loading plugins.
let mapleader=','

" Vundle.
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'

" Visual.
Plugin 'w0ng/vim-hybrid'
Plugin 'bling/vim-airline'

" Movement.
Plugin 'Lokaltog/vim-easymotion'
Plugin 'tpope/vim-unimpaired'
Plugin 'kien/ctrlp.vim'
Plugin 'FelikZ/ctrlp-py-matcher'
Plugin 'vim-scripts/bufkill.vim'

" Coding.
" Plugin 'scrooloose/nerdcommenter'
Plugin 'tpope/vim-commentary'
Plugin 'scrooloose/syntastic'

" VCS.
Plugin 'tpope/vim-fugitive'
Plugin 'mhinz/vim-signify'

" Text.
Plugin 'tpope/vim-surround'
" Plugin 'godlygeek/tabular'

" Plugins.
Plugin 'aaronbieber/vim-quicktask'
Plugin 'sjl/gundo.vim'

" Misc.
Plugin 'tpope/vim-sensible'
Plugin 'tpope/vim-repeat'

" Disabled.
" Plugin 'Shougo/vimproc.vim'
" Plugin 'Shougo/unite.vim'
" Plugin 'pydave/AsyncCommand'
" Plugin 'stgpetrovic/syntastic-async'

" Haskell.
"Plugin "Twinside/vim-hoogle"
"Plugin 'dag/vim2hs'
"Plugin 'eagletmt/ghcmod-vim'
"Plugin 'bitc/vim-hdevtools'
"Plugin 'jpalardy/vim-slime'
call vundle#end()

" Google.
if filereadable(glob('~/.vim/google.vim'))
  source ~/.vim/google.vim
endif

" End of plugins.
filetype plugin indent on
syntax on

" Behavior.
set ignorecase
set smartcase
set wildmode=longest,list:longest,full
set nobackup
set nowb
set noswapfile
set shortmess=at

" Indentation.
set expandtab
set tabstop=2     " Tab is 2 spaces
set softtabstop=2 " Deletion at an initial tab will remove 2 spaces
set shiftwidth=2  " Number of spaces to use for autoindenting
set shiftround    " Use multiple of shiftwidth when indenting with '<' and '>'
set autoindent    " Always set autoindenting on
"set copyindent    " copy the previous indentation on autoindenting
set smartindent

" autocmd InsertLeave * if pumvisible() == 0|pclose|endif
" autocmd BufWritePre * :%s/\s\+$//e
" autocmd Syntax * normal zR

" Appearance.
set number
set novisualbell
set noerrorbells
set cursorline
set colorcolumn=+1
set hlsearch
set hidden
set t_Co=256
set background=dark
let g:hybrid_use_Xresources = 1
colorscheme hybrid

" GUI-only
if has('gui_running')
  set guiheadroom=0
  set guioptions=aceit
  let s:uname = system("uname -s")
  if s:uname == "Darwin\n"
    set guifont=Inconsolata:h15
    set clipboard=unnamed
  else
    set guifont=Anonymous\ Pro\ 10
    set clipboard=unnamedplus
  endif
  set vb
endif

" Splits.
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
set splitbelow
set splitright

" ctrlp
" nunmap <C-b>
nnoremap <silent> <C-b> :CtrlPBuffer<CR>
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_user_command = 'ag %s -i --nocolor --nogroup --hidden
      \ --ignore .git
      \ --ignore .svn
      \ --ignore .hg
      \ --ignore .DS_Store
      \ --ignore "**/*.pyc"
      \ --ignore .git5_specs
      \ --ignore review
      \ -g ""'
let g:ctrlp_match_func = { 'match': 'pymatcher#PyMatch' }

" Tabularize.
if exists(":Tabularize")
  nmap <Leader>a= :Tabularize /=<CR>
  vmap <Leader>a= :Tabularize /=<CR>
  nmap <Leader>a: :Tabularize /:\zs<CR>
  vmap <Leader>a: :Tabularize /:\zs<CR>
endif

" synatastic
"let g:syntastic_enable_signs=1
"map <silent> <Leader>e :Errors<CR>
"map <Leader>s :SyntasticToggleMode<CR>
"let g:syntastic_auto_loc_list=1
"let g:syntastic_cpp_compiler_options = ' -std=c++11'

" Haskell stuff.
" map <silent> tu :call GHC_BrowseAll()<CR>
" map <silent> tw :call GHC_ShowType(1)<CR>
" let g:slime_target = "tmux"
" let g:slime_paste_file = tempname()

" vim2hs
" let g:hpaste_author = 'step_function'

" hdevtools
"au FileType haskell nnoremap <buffer> <Leader>ht :HdevtoolsType<CR>
"au FileType haskell nnoremap <buffer> <silent> <Leader>hc :HdevtoolsClear<CR>
"au FileType haskell nnoremap <buffer> <silent> <Leader>hi :HdevtoolsInfo<CR>

" airline
let g:airline_left_sep=''
let g:airline_right_sep=''

" Commentary.
" setglobal commentstring=#\ %s
" autocmd FileType c,cpp,cs   setlocal commentstring=//\ %s
" autocmd FileType java       setlocal commentstring=/*%s*/

" Gundo.
nnoremap <silent> <leader>g :GundoToggle<CR>
nnoremap <F5> :GundoToggle<CR>
let g:gundo_right = 1

" Signify.
let g:signify_disable_by_default = 1
let g:signify_vcs_list = ['git5', 'git']
let g:signify_diffoptions = {'git5': '--uncommitted'}
nnoremap <Leader>vt :SignifyToggle<CR>

