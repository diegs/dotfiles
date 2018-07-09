if &compatible
 set nocompatible
endif

set runtimepath+=~/.local/share/dein/repos/github.com/Shougo/dein.vim

if dein#load_state('~/.local/share/dein')
  call dein#begin('~/.local/share/dein')
  call dein#add('~/.local/share/dein')

  " Navigation.
  call dein#add('ctrlpvim/ctrlp.vim')
  call dein#add('FelikZ/ctrlp-py-matcher')
  call dein#add('jremmen/vim-ripgrep')

  " Visual.
  call dein#add('chriskempson/base16-vim')
  call dein#add('vim-airline/vim-airline')
  call dein#add('vim-airline/vim-airline-themes')

  " Completion.
  call dein#add('Shougo/deoplete.nvim')
  if !has('nvim')
    call dein#add('roxma/nvim-yarp')
    call dein#add('roxma/vim-hug-neovim-rpc')
  endif
  call dein#add('autozimu/LanguageClient-neovim', {
    \ 'rev': 'next',
    \ 'build': 'bash install.sh',
    \ })

  " Overrides.
  call dein#add('tpope/vim-vinegar')

  " Languages.
  call dein#add('sheerun/vim-polyglot')
  call dein#add('saltstack/salt-vim')

  " Formatting.
  call dein#add('w0rp/ale')
  call dein#add('tpope/vim-repeat')
  call dein#add('tpope/vim-abolish')
  call dein#add('tpope/vim-commentary')
  call dein#add('tpope/vim-repeat')
  call dein#add('tpope/vim-unimpaired')
  call dein#add('kana/vim-textobj-user')
  call dein#add('glts/vim-textobj-comment')

  " Integrations.
  call dein#add('christoomey/vim-tmux-navigator')
  call dein#add('tmux-plugins/vim-tmux-focus-events')
  call dein#add('janko-m/vim-test')

 call dein#end()
 call dein#save_state()
endif

filetype plugin indent on
syntax enable

" Settings.
set hidden
set showmatch
set ignorecase
set smartcase
set nobackup
set nowb
set noswapfile
set autoread

" Editing.
set tabstop=2
set softtabstop=2
set expandtab
set shiftwidth=2
set autoindent
set wildmode=longest:list
" set wildmode=longest:full
"set wildmenu
set nojoinspaces
set matchpairs+=<:>

" UI.
set hlsearch
set number
set relativenumber
set splitbelow
set splitright
set lazyredraw
if filereadable(expand("~/.vimrc_background"))
  let base16colorspace=256
  source ~/.vimrc_background
  let g:airline_theme='base16_shell'
endif

" Clipboard.
set clipboard=unnamed

" Spelling.
set nospell

" Navigation.
nnoremap <silent> <C-b> :CtrlPBuffer<CR>
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_switch_buffer = ''
let g:ctrlp_match_func = { 'match': 'pymatcher#PyMatch' }
if executable('rg')
  let g:ctrlp_user_command = 'rg %s --files --color=never --glob ""'
  let g:ctrlp_use_caching = 0
else
  let g:ctrlp_clear_cache_on_exit = 0
endif
let g:rg_highlight = 1
let g:rg_derive_root = 1

" LSP.
let g:LanguageClient_serverCommands = {
\  'go': ['go-langserver', '-gocodecompletion'],
\  'haskell': ['hie', '--lsp'],
\  'python': ['pyls'],
\ }
nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>
let g:LanguageClient_diagnosticsEnable = 0

" ALE.
let g:ale_fixers = {
\  'go': ['goimports'],
\}
let g:ale_fix_on_save = 1
let g:airline#extensions#ale#enabled = 1
let g:ale_set_highlights = 0
let g:ale_sign_column_always = 1
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_insert_leave = 1
let g:ale_completion_enabled = 0

" Completion.
let g:deoplete#enable_at_startup = 1
inoremap <expr><C-h> deoplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS>  deoplete#smart_close_popup()."\<C-h>"
set completeopt-=preview
call deoplete#custom#option({
\ 'camel_case': v:true,
\ 'sources': {
\   'go': ['LanguageClient'],
\   'python': ['LanguageClient'],
\ },
\ })
call deoplete#custom#source('_', 'matchers', ['matcher_fuzzy', 'matcher_length'])
call deoplete#custom#source('_', 'disabled_syntaxes', ['Comment', 'String'])

" Go.
au FileType go set noexpandtab
au FileType go set tw=120

" Python
au FileType python set tabstop=4
au FileType python set shiftwidth=4
au FileType python set tw=120

" netrw
"let g:netrw_liststyle = 3
" let g:netrw_browse_split = 4
" let g:netrw_altv = 1
" let g:netrw_winsize = 25
" let g:netrw_banner = 0
" let g:netrw_list_hide = &wildignore
" autocmd FileType netrw setl bufhidden=delete
