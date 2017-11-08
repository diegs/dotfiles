" For plugin loading.
set nocompatible
filetype off
syntax off

" First things first I'm the lead-est.
let mapleader=','

" vim-plug.
call plug#begin('~/.vim/plugged')

" Meta.
Plug 'tpope/vim-sensible'
Plug 'starcraftman/cmdalias.vim'

" Navigation.
Plug 'ctrlpvim/ctrlp.vim'
Plug 'FelikZ/ctrlp-py-matcher'
Plug 'vim-scripts/bufkill.vim'
Plug 'schickling/vim-bufonly'
Plug 'jeetsukumaran/vim-filebeagle'
Plug 'majutsushi/tagbar'
Plug 'ivalkeen/vim-ctrlp-tjump'
Plug 'let-def/vimbufsync'
Plug 'christoomey/vim-tmux-navigator'

" Visual.
Plug 'chriskempson/base16-vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Text.
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-surround'
Plug 'godlygeek/tabular'
Plug 'rking/ag.vim'
Plug 'roxma/vim-paste-easy'
Plug 'kana/vim-textobj-user'
Plug 'Julian/vim-textobj-variable-segment'

" Coding.
Plug 'tomtom/tcomment_vim'
Plug 'ConradIrwin/vim-comment-object'
Plug 'b4winckler/vim-angry'

" Organization.
" Plug 'jceb/vim-orgmode'
" Plug 'vimwiki/vimwiki'

" Completion.
Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/asyncomplete-gocode.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'prabirshrestha/asyncomplete-lsp.vim'

" Languages.
Plug 'jvoorhis/coq.vim'
Plug 'the-lambda-church/coquille'
Plug 'neovimhaskell/haskell-vim'
Plug 'rust-lang/rust.vim'
Plug 'google/vim-ft-bzl'
Plug 'fatih/vim-go'
Plug 'LnL7/vim-nix'
Plug 'cespare/vim-toml'

call plug#end()

" End of plugins.
filetype plugin indent on
syntax on

" Behavior.
set ignorecase
set smartcase
set nojoinspaces
set wildmode=longest,list:longest,full
set nobackup
set nowb
set noswapfile
set spell
set ruler
set matchpairs+=<:>
command! Q q
command! W w
command! WA wa
nnoremap Y y$
nnoremap <Leader>w :w<CR>
nnoremap <Leader>q :q<CR>

" Indentation.
set expandtab
set tabstop=2     " Tab is 2 spaces
set softtabstop=2 " Deletion at an initial tab will remove 2 spaces
set shiftwidth=2  " Number of spaces to use for autoindenting
set shiftround    " Use multiple of shiftwidth when indenting with '<' and '>'
set autoindent    " Always set autoindenting on
set copyindent    " copy the previous indentation on autoindenting
set smartindent

" Appearance.
set relativenumber
" set number
set showcmd
set novisualbell
set noerrorbells
set cursorline
set lazyredraw
set showmatch
set colorcolumn=+1
set hlsearch
set incsearch
set hidden
autocmd VimResized * :wincmd =

"set background=dark
if filereadable(expand("~/.vimrc_background"))
	let base16colorspace=256
	source ~/.vimrc_background
endif

"set clipboard=unnamed,unnamedplus
" if (executable('pbcopy') || executable('xclip') || executable('xsel'))
"   if has('unnamedplus')
"     set clipboard=unnamed,unnamedplus
"   else
"     set clipboard=unnamed
"   endif
" endif

let s:uname = system("uname -s")
if has('gui_running')
	set guiheadroom=0
	set guioptions=aceit
	if s:uname == "Darwin\n"
		set guifont=Inconsolata:h15,Menlo:h14
	else
		" set guifont=Anonymous\ Pro\ 10
		set guifont=Envy\ Code\ R\ 10
	endif
	set clipboard=unnamedplus
else
	set mouse=
endif

" Go
let g:go_fmt_command = "gofmt"
let g:go_fmt_options = {
			\ 'gofmt': '-s',
			\ 'goimports': '-local github.com/coreos',
			\ }

" Rust
let g:rustfmt_autosave = 1

" Coquille
nnoremap <silent> <leader>cn :CoqNext<CR>
nnoremap <silent> <leader>cu :CoqUndo<CR>
nnoremap <silent> <leader>cc :CoqToCursor<CR>
nnoremap <silent> <leader>ck :CoqKill<CR>
let g:coquille_auto_move="true"
hi link CheckedByCoq Folded
hi link SentToCoq Folded

" Splits.
" nnoremap <C-J> <C-W><C-J>
" nnoremap <C-K> <C-W><C-K>
" nnoremap <C-L> <C-W><C-L>
" nnoremap <C-H> <C-W><C-H>
set splitbelow
set splitright
" if has('nvim')
"   " Fix for C-h in nvim.
"   nmap <silent> <bs> :<c-u>TmuxNavigateLeft<cr>
" endif

" CtrlP.
" nunmap <C-b>
let g:ctrlp_use_caching = 0
nnoremap <silent> <C-b> :CtrlPBuffer<CR>
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_switch_buffer = ''
set wildignore+=*/.git/*,*.swp
if executable('rg')
	set grepprg=rg\ --color=never
	let g:ctrlp_user_command = 'rg %s --files --color=never --glob ""'
	" else
	"   let g:ctrlp_user_command = 'ag %s -i --nocolor --nogroup --hidden
	"         \ --ignore .git
	"         \ --ignore .svn
	"         \ --ignore .hg
	"         \ --ignore .cache
	"         \ --ignore .config
	"         \ --ignore .local
	"         \ --ignore .gnome
	"         \ --ignore .gradle
	"         \ --ignore .stack-work
	"         \ --ignore .vagrant
	"         \ --ignore dist
	"         \ --ignore out
	"         \ --ignore vendor
	"         \ -g ""'
endif
let g:ctrlp_match_func = { 'match': 'pymatcher#PyMatch' }

" Tabularize.
if exists(":Tabularize")
	nmap <Leader>a= :Tabularize /=<CR>
	vmap <Leader>a= :Tabularize /=<CR>
	nmap <Leader>a: :Tabularize /:\zs<CR>
	vmap <Leader>a: :Tabularize /:\zs<CR>
endif

" Slime.
" let g:slime_target = "tmux"
" let g:slime_paste_file = tempname()
"let g:slime_no_mappings = 1
"xmap <leader>s <Plug>SlimeRegionSend
"nmap <leader>s <Plug>SlimeMotionSend
"nmap <leader>ss <Plug>SlimeLineSend

" Airline.
" let g:airline_left_sep=''
" let g:airline_right_sep=''
" let g:airline_section_z=''
let g:airline_theme = 'base16_shell'

" vim-pad.
let g:pad#dir = '~/txt'
let g:pad#search_backend = 'ag'
let g:pad#open_in_split = 0
let g:pad#default_file_extension = '.md'

" Wiki.
let g:vimwiki_list = [{'path': '~/txt'}]

" Tagbar.
let $TMPDIR = "/tmp"
map <leader>t :TagbarToggle<CR>
let g:tagbar_type_go = {
			\ 'ctagstype': 'go',
			\ 'kinds' : [
			\'p:package',
			\'f:function',
			\'v:variables',
			\'t:type',
			\'c:const'
			\]
			\}
let g:tagbar_type_make = {
			\ 'kinds':[
			\ 'm:macros',
			\ 't:targets'
			\ ]
			\}
let g:tagbar_type_markdown = {
			\ 'ctagstype' : 'markdown',
			\ 'kinds' : [
			\ 'h:Heading_L1',
			\ 'i:Heading_L2',
			\ 'k:Heading_L3'
			\ ]
			\ }

" Cmdalias.
call cmdalias#add('bd', 'BD')
call cmdalias#add('ag', 'Ag')

cnoremap sudow w !sudo tee % >/dev/null

" Asynccomplete.
if executable('gocode')
	call asyncomplete#register_source(asyncomplete#sources#gocode#get_source_options({
				\ 'name': 'gocode',
				\ 'whitelist': ['go'],
				\ 'completor': function('asyncomplete#sources#gocode#completor'),
				\ }))
endif

if executable('go-langserver')
	au User lsp_setup call lsp#register_server({
				\ 'name': 'go-langserver',
				\ 'cmd': {server_info->['go-langserver', '-mode', 'stdio']},
				\ 'whitelist': ['go'],
				\ })
endif

if executable('rls')
	au User lsp_setup call lsp#register_server({
				\ 'name': 'rls',
				\ 'cmd': {server_info->['rustup', 'run', 'nightly', 'rls']},
				\ 'whitelist': ['rust'],
				\ })
endif

inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<cr>"
imap <c-space> <Plug>(asyncomplete_force_refresh)
let g:asyncomplete_auto_popup = 1
autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif

" LSP
nnoremap <silent> <leader>gd :LspDefinition<CR>
nnoremap <silent> <leader>gh :LspHover<CR>
nnoremap <silent> <leader>gr :LspReferences<CR>
