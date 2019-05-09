let mapleader = ','

call plug#begin('~/.local/share/nvim/plugged')

" Navigation.
Plug '/home/diegs/.nix-profile/share/vim-plugins/fzf-vim'
Plug '/home/diegs/.nix-profile/share/vim-plugins/fzf'
Plug 'junegunn/fzf.vim'
Plug 'majutsushi/tagbar'
Plug 'https://github.com/qpkorr/vim-bufkill'

" Visual.
Plug 'danielwe/base16-vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'haya14busa/vim-asterisk'

" Overrides.
Plug 'tpope/vim-vinegar'

" Languages.
Plug 'sheerun/vim-polyglot'
Plug 'saltstack/salt-vim'

" Formatting.
Plug 'w0rp/ale'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'junegunn/vim-easy-align'
Plug 'kana/vim-textobj-user'
Plug 'glts/vim-textobj-comment'

" Integrations.
Plug 'christoomey/vim-tmux-navigator'
Plug 'tmux-plugins/vim-tmux-focus-events'
Plug 'roxma/vim-tmux-clipboard'
Plug 'tpope/vim-fugitive'

call plug#end()

filetype plugin indent on
syntax enable

" Settings.
set noshowcmd
set noruler
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
set nojoinspaces
set matchpairs+=<:>

" UI.
set hlsearch
set incsearch
set number
set relativenumber
set splitbelow
set splitright

if filereadable(expand("~/.vimrc_background"))
  let base16colorspace=256
  source ~/.vimrc_background
  let g:airline_theme='base16_shell'
endif

" Clipboard.
" set clipboard=unnamed

" Spelling.
set spell
highlight SpellBad cterm=undercurl ctermbg=238 gui=undercurl guisp=#F07178
highlight Comment ctermfg=gray
" highlight clear SpellCap

" FZF.
let g:fzf_command_prefix = 'Fzf'
let g:fzf_buffers_jump = 1
nnoremap <silent> <C-p> :FzfFiles<CR>
nnoremap <silent> <C-b> :FzfBuffers<CR>
nnoremap <silent> <leader>r :FzfRg<CR>

" Asterisk.
map * <Plug>(asterisk-z*)
map # <Plug>(asterisk-z#)
map g* <Plug>(asterisk-gz*)
map g# <Plug>(asterisk-gz#)
let g:asterisk#keeppos = 1

" ALE.
let g:ale_linters = {
\  'go': ['gopls'],
\  'markdown': ['prettier'],
\  'python': ['flake8', 'mypy', 'pylint', 'pyls'],
\  'rust': ['rls'],
\}
let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\  'go': ['goimports', 'gofmt'],
\  'rust': ['rustfmt'],
\}
" \  'markdown': ['prettier'],

autocmd BufNewFile,BufRead ~/src/github.com/lyft/dispatch/* let b:ale_fixers = {'python': ['black']}
autocmd BufNewFile,BufRead ~/src/github.com/lyft/marketstate/* let b:ale_fixers = {'python': ['black']}

let g:ale_go_gofmt_options = '-s'
let g:ale_go_gobuild_options = '-tags integration'
"let g:ale_go_golangci_lint_options = '--fast -c ~/.golangci.yml '
let g:ale_go_golangci_lint_package = 1
let g:ale_javascript_prettier_options = '--no-bracket-spacing'
let g:ale_rust_rls_toolchain = 'stable'
let g:ale_fix_on_save = 1
let g:ale_set_balloons = 1
let g:airline#extensions#ale#enabled = 1
let g:ale_set_highlights = 0
let g:ale_sign_column_always = 1
let g:ale_completion_enabled = 1
nnoremap <silent> <leader>h :ALEHover<CR>
nnoremap <silent> <leader>g :ALEGoToDefinition<CR>
inoremap <silent><expr> <Tab> pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <silent><expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-TAB>"

" Go.
au FileType go set noexpandtab
au FileType go set tw=120

" Python
au FileType python set tabstop=4
au FileType python set shiftwidth=4
au FileType python set tw=120

" Git
au FileType gitcommit au! BufEnter COMMIT_EDITMSG call setpos('.', [0, 1, 1, 0])

" Tagbar.
nnoremap <leader>to :TagbarToggle<CR>
nnoremap <leader>tp :TagbarTogglePause<CR>
let g:tagbar_left = 1
let g:tagbar_type_go = {
  \ 'ctagstype' : 'go',
  \ 'kinds'     : [
    \ 'p:package',
    \ 'i:imports:1',
    \ 'c:constants',
    \ 'v:variables',
    \ 't:types',
    \ 'n:interfaces',
    \ 'w:fields',
    \ 'e:embedded',
    \ 'm:methods',
    \ 'r:constructor',
    \ 'f:functions'
  \ ],
  \ 'sro' : '.',
  \ 'kind2scope' : {
    \ 't' : 'ctype',
    \ 'n' : 'ntype'
  \ },
  \ 'scope2kind' : {
    \ 'ctype' : 't',
    \ 'ntype' : 'n'
  \ },
  \ 'ctagsbin'  : 'gotags',
  \ 'ctagsargs' : '-sort -silent'
  \ }

" Easy-align.
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" BufferDelete.
function! CommandCabbr(abbreviation, expansion)
  execute 'cabbr ' . a:abbreviation . ' <c-r>=getcmdpos() == 1 && getcmdtype() == ":" ? "' . a:expansion . '" : "' . a:abbreviation . '"<CR>'
endfunction
command! -nargs=+ CommandCabbr call CommandCabbr(<f-args>)

CommandCabbr bd BD
nnoremap <silent> <leader>d :BD<CR>
