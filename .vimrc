" For plugin loading.
set nocompatible
filetype off

syntax off

" First things first I'm the lead-est.
let mapleader=','

" vim-plug.
call plug#begin('~/.vim/plugged')

" Visual.
" Plug 'w0ng/vim-hybrid'
Plug 'chriskempson/base16-vim'
" Plug 'ajh17/Spacegray.vim'
" Plug 'NLKNguyen/papercolor-theme'
" Plug 'jordwalke/flatlandia'
" Plug 'romainl/Apprentice'
" Plug 'noahfrederick/vim-hemisu'
" Plug 'noahfrederick/vim-noctu'
" Plug 'morhetz/gruvbox'
" Plug 'bling/vim-airline'

" Movement.
" Plug 'Lokaltog/vim-easymotion'
Plug 'tpope/vim-unimpaired'
Plug 'kien/ctrlp.vim'
Plug 'FelikZ/ctrlp-py-matcher'
Plug 'vim-scripts/bufkill.vim'
Plug 'schickling/vim-bufonly'
" Plug 'unblevable/quick-scope'

" Coding.
" Plug 'scrooloose/nerdcommenter'
" Plug 'tpope/vim-commentary'
Plug 'tomtom/tcomment_vim'
" Plug 'scrooloose/syntastic'
Plug 'b4winckler/vim-angry'
Plug 'vasconcelloslf/vim-interestingwords'

" VCS.
" Plug 'tpope/vim-fugitive'
" Plug 'mhinz/vim-signify'

" Text.
Plug 'tpope/vim-surround'
Plug 'godlygeek/tabular'
Plug 'rking/ag.vim'

" Plugs.
Plug 'fmoralesc/vim-pad'
Plug 'aaronbieber/vim-quicktask'
Plug 'jeetsukumaran/vim-filebeagle'
" Plug 'xolox/vim-misc'
" Plug 'xolox/vim-easytags'
" Plug 'majutsushi/tagbar'
" Plug 'sjl/gundo.vim'
Plug 'vim-scripts/YankRing.vim'

" Misc.
" Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sensible'

" Tmux.
" Plug 'tmux-plugins/vim-tmux-focus-events'
Plug 'jpalardy/vim-slime'
Plug 'christoomey/vim-tmux-navigator'

" Disabled.
" Plug 'Shougo/vimproc.vim'
" Plug 'Shougo/unite.vim'
" Plug 'pydave/AsyncCommand'
" Plug 'stgpetrovic/syntastic-async'

" Languages.
Plug 'raichoo/haskell-vim'
Plug 'rust-lang/rust.vim'
Plug 'cespare/vim-toml'
" Plug 'lukerandall/haskellmode-vim'
" Plug 'Twinside/vim-hoogle'
" Plug 'dag/vim2hs'
" Plug 'eagletmt/ghcmod-vim'
" Plug 'bitc/vim-hdevtools'
call plug#end()

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
" set shortmess=at
" inoremap jk <esc>
" nnoremap ho :noh<CR>
set relativenumber
command! Q q
command! W w
command! WA wa
nnoremap Y y$
nnoremap <Leader>w :w<CR>

" Indentation.
set expandtab
set tabstop=2     " Tab is 2 spaces
set softtabstop=2 " Deletion at an initial tab will remove 2 spaces
set shiftwidth=2  " Number of spaces to use for autoindenting
set shiftround    " Use multiple of shiftwidth when indenting with '<' and '>'
set autoindent    " Always set autoindenting on
set copyindent    " copy the previous indentation on autoindenting
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

" if strftime("%H") > 17
set background=dark
colorscheme base16-ocean

if has('unnamedplus')
  set clipboard=unnamed,unnamedplus
else
  set clipboard=unnamed
endif

let s:uname = system("uname -s")
if has('gui_running')
  set guiheadroom=0
  set guioptions=aceit
  if s:uname == "Darwin\n"
    set guifont=Inconsolata:h15,Menlo:h14
  else
    set guifont=Anonymous\ Pro\ 10
  endif
endif

" Splits.
" nnoremap <C-J> <C-W><C-J>
" nnoremap <C-K> <C-W><C-K>
" nnoremap <C-L> <C-W><C-L>
" nnoremap <C-H> <C-W><C-H>
set splitbelow
set splitright

" CtrlP.
" nunmap <C-b>
nnoremap <silent> <C-b> :CtrlPBuffer<CR>
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_switch_buffer = 'VH'
let g:ctrlp_user_command = 'ag %s -i --nocolor --nogroup --hidden
      \ --ignore .git
      \ --ignore .svn
      \ --ignore .hg
      \ --ignore .DS_Store
      \ --ignore .cache
      \ --ignore .config
      \ --ignore "**/*.pyc"
      \ --ignore dist
      \ --ignore out
      \ --ignore .stack-work
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
let g:syntastic_auto_loc_list=1
"let g:syntastic_cpp_compiler_options = ' -std=c++11'
let g:syntastic_always_populate_loc_list = 1

" Slime.
let g:slime_target = "tmux"
let g:slime_paste_file = tempname()
"let g:slime_no_mappings = 1
"xmap <leader>s <Plug>SlimeRegionSend
"nmap <leader>s <Plug>SlimeMotionSend
"nmap <leader>ss <Plug>SlimeLineSend

" vim2hs
" let g:hpaste_author = 'step_function'

" Haskell.
" let g:haddock_browser="open"
" au FileType haskell nnoremap <buffer> <Leader>ht :HdevtoolsType<CR>
" au FileType haskell nnoremap <buffer> <silent> <Leader>hc :HdevtoolsClear<CR>
" au FileType haskell nnoremap <buffer> <silent> <Leader>hi :HdevtoolsInfo<CR>
" map <silent> tu :call GHC_BrowseAll()<CR>
" map <silent> tw :call GHC_ShowType(1)<CR>

" Airline.
let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline_section_z=''

" Buffer selector.
nmap <leader>b :ls<CR>:b<space>

" Commentary.
" setglobal commentstring=#\ %s
" autocmd FileType c,cpp,cs   setlocal commentstring=//\ %s
" autocmd FileType java       setlocal commentstring=/*%s*/

" Nerdtree.
" map <leader>n :NERDTreeToggle<CR>

" Netrw
" let g:netrw_altfile = 1

" vim-pad.
let g:pad#dir = '~/txt'

" Tagbar.
" map <leader>t :TagbarToggle<CR>

" Gundo.
" nnoremap <silent> <leader>g :GundoToggle<CR>
" nnoremap <F5> :GundoToggle<CR>
" let g:gundo_right = 1

" Yankring.
if has('gui_running')
  let g:yankring_replace_n_pkey = '<m-p>'
  let g:yankring_replace_n_nkey = '<m-n>'
else
  let g:yankring_replace_n_pkey = '<esc>p'
  let g:yankring_replace_n_nkey = '<esc>n'
endif
let g:yankring_history_dir = '~/.vim'

" Signify.
" let g:signify_disable_by_default = 1
" let g:signify_vcs_list = ['git5', 'git']
" let g:signify_diffoptions = {'git5': '--uncommitted'}
" nnoremap <Leader>vt :SignifyToggle<CR>

" Highlight all instances of word under cursor, when idle.
" Useful when studying strange source code.
" Type z/ to toggle highlighting on/off.
nnoremap <leader>h :if AutoHighlightToggle()<Bar>set hls<Bar>endif<CR>
function! AutoHighlightToggle()
  let @/ = ''
  if exists('#auto_highlight')
    au! auto_highlight
    augroup! auto_highlight
    setl updatetime=4000
    echo 'Highlight current word: off'
    return 0
  else
    augroup auto_highlight
      au!
      au CursorHold * let @/ = '\V\<'.escape(expand('<cword>'), '\').'\>'
    augroup end
    setl updatetime=500
    echo 'Highlight current word: on'
    return 1
  endif
endfunction
