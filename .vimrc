if !1 | finish | endif
if has('vim_starting')
  set nocompatible
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

" google
if filereadable("~/.vim/google.vim")
  source ~/.vim/google.vim
endif

" neobundle
set rtp+=~/.vim/bundle/neobundle.vim
call neobundle#begin(expand('~/.vim/bundle/'))
NeoBundleFetch 'Shuogo/neobundle.vim'
NeoBundle 'tpope/vim-sensible'
NeoBundle 'tpope/vim-surround'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'bling/vim-airline'
"NeoBundle 'scrooloose/syntastic'
NeoBundle 'Shougo/vimproc.vim'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'godlygeek/tabular'
NeoBundle 'aaronbieber/vim-quicktask'
NeoBundle 'vim-scripts/bufkill.vim'
NeoBundle 'altercation/vim-colors-solarized'
NeoBundle 'kien/ctrlp.vim'
NeoBundle 'FelikZ/ctrlp-py-matcher'
NeoBundle 'scrooloose/nerdcommenter'

" Haskell plugins.
"NeoBundle "Twinside/vim-hoogle"
NeoBundle 'dag/vim2hs'
"NeoBundle 'eagletmt/ghcmod-vim'
"NeoBundle 'bitc/vim-hdevtools'
"NeoBundle 'jpalardy/vim-slime'
call neobundle#end()

filetype plugin indent on
NeoBundleCheck

" Standard customizations.
" filetype plugin indent on
let mapleader=','
" set wildmode=list:longest
set wildmenu
set wildmode=longest:full,full
autocmd BufWritePre * :%s/\s\+$//e
autocmd Syntax * normal zR
set nobackup
set nowb
set noswapfile
autocmd InsertLeave * if pumvisible() == 0|pclose|endif

" Tab settings.
set tabstop=2                   "A tab is 8 spaces
set expandtab                   "Always uses spaces instead of tabs
set softtabstop=2               "Insert 2 spaces when tab is pressed
set shiftwidth=2                "An indent is 2 spaces
set shiftround                  "Round spaces to nearest shiftwidth multiple
set nojoinspaces                "Don't convert spaces to tabs

" Appearance.
set number
set cursorline
set hlsearch
set hidden
set t_Co=256
set background=dark
colorscheme solarized

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

" GUI-only
if has('gui_running')
  set guiheadroom=0
  set guioptions=aceit
  set guifont=Inconsolata:h15
  set vb
endif

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
