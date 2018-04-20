" For plugin loading.
set nocompatible
filetype off
syntax off

" First things first I'm the lead-est.
let mapleader=','

" vim-plug.
call plug#begin('~/.vim/plugged')

" Navigation.
Plug 'ctrlpvim/ctrlp.vim'
Plug 'FelikZ/ctrlp-py-matcher'
Plug 'jremmen/vim-ripgrep'
Plug 'justinmk/vim-dirvish'
Plug 'christoomey/vim-tmux-navigator'

" Visual.
Plug 'chriskempson/base16-vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Completion.
Plug 'prabirshrestha/async.vim'
" Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/vim-lsp'
" Plug 'prabirshrestha/asyncomplete-lsp.vim'
" Plug 'prabirshrestha/asyncomplete-gocode.vim'

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'zchee/deoplete-go', { 'do': 'make'}
Plug 'fatih/vim-go'
Plug 'saltstack/salt-vim'

" Formatting.
" Plug 'w0rp/ale'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-repeat'

" General text manipulation.
Plug 'tpope/vim-commentary'

call plug#end()

" End of plugins.
filetype plugin indent on
syntax on

" Vim behavior.
set nobackup
set nowb
set noswapfile
set splitbelow
set splitright

" Appearance.
set ruler
set relativenumber
set number
set novisualbell
set noerrorbells
set cursorline
set colorcolumn=+1
set hlsearch
set incsearch
set hidden

" Text behavior.
set nojoinspaces
set matchpairs+=<:>

" Spelling.
set spell
autocmd FileType dirvish setlocal nospell

" Navigation.
nnoremap <silent> <C-b> :CtrlPBuffer<CR>
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_switch_buffer = ''
let g:ctrlp_match_func = { 'match': 'pymatcher#PyMatch' }
if executable('rg')
  "set grepprg=rg\ --color=never
  let g:ctrlp_user_command = 'rg %s --files --color=never --glob ""'
  let g:ctrlp_use_caching = 0
else
  let g:ctrlp_clear_cache_on_exit = 0
endif

" Visual.
colorscheme base16-materia

let g:go_def_mapping_enabled = 0
let g:go_fmt_command = 'goimports'
let g:go_fmt_fail_silently = 0
let g:go_term_enabled = 1
let g:go_auto_sameids = 0

" inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
" inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
" inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<cr>"
" imap <c-space> <Plug>(asyncomplete_force_refresh)

let g:deoplete#enable_at_startup = 1
let g:deoplete#sources#go#sort_class = ['package', 'func', 'type', 'var', 'const']

if executable('pyls')
	au User lsp_setup call lsp#register_server({
				\ 'name': 'pyls',
				\ 'cmd': {server_info->['pyls']},
				\ 'whitelist': ['python'],
				\ })
endif

if executable('go-langserver')
	au User lsp_setup call lsp#register_server({
				\ 'name': 'go-langserver',
				\ 'cmd': {server_info->['go-langserver', '-mode', 'stdio']},
				\ 'whitelist': ['go'],
				\ })
endif

" call asyncomplete#register_source(asyncomplete#sources#gocode#get_source_options({
"     \ 'name': 'gocode',
"     \ 'whitelist': ['go'],
"     \ 'completor': function('asyncomplete#sources#gocode#completor'),
"     \ }))

nnoremap <silent> gh :LspHover<CR>
nnoremap <silent> gd :LspDefinition<CR>
nnoremap <silent> gr :LspReferences<CR>

let g:lsp_signs_enabled = 1
let g:lsp_diagnostics_echo_cursor = 1
set completeopt-=preview
