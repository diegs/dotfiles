let mapleader = ','

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
set spell
set clipboard=unnamed

" UI.
set hlsearch
set incsearch
set number
set relativenumber
set splitbelow
set splitright
set noshowmode
augroup MyColors
  autocmd!
  autocmd ColorScheme * highlight SpellBad cterm=undercurl ctermbg=238 gui=undercurl guisp=#F07178
                    \ | highlight Comment ctermfg=gray
                    \ | highlight clear SpellCap
augroup END
if filereadable(expand("~/.vimrc_background"))
  let base16colorspace=256
  source ~/.vimrc_background
endif

" Airline
let g:airline_theme='base16_shell'

" FZF.
let g:fzf_command_prefix = 'Fzf'
" let g:fzf_buffers_jump = 1
nnoremap <silent> <C-p> :FzfFiles<CR>
nnoremap <silent> <C-b> :FzfBuffers<CR>
nnoremap <silent> <leader>r :FzfRg<CR>
autocmd! FileType fzf
autocmd  FileType fzf set laststatus=0 noshowmode noruler
  \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler

" ALE.
let g:ale_linters = {
\  'go': ['gopls'],
\  'markdown': ['prettier'],
\  'python': ['pyls'],
\  'rust': ['rls'],
\}
" \  'python': ['flake8', 'mypy', 'pylint', 'pyls'],
let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\  'go': ['goimports', 'gofmt'],
\  'python': [],
\  'rust': ['rustfmt'],
\}
autocmd BufNewFile,BufRead ~/src/github.com/lyft/dispatch/* let b:ale_fixers = {'python': ['black', 'reorder-python-imports']}
autocmd BufNewFile,BufRead ~/src/github.com/lyft/ridescheduler/* let b:ale_fixers = {'python': ['black', 'reorder-python-imports']}
let g:ale_completion_enabled = 1
let g:ale_fix_on_save = 1
let g:ale_go_gofmt_options = '-s'
let g:ale_go_golangci_lint_options = '--fast -c ~/.golangci.yml '
let g:ale_go_golangci_lint_package = 1
let g:ale_lint_on_insert_leave = 1
let g:ale_lint_on_text_changed = 'normal'
let g:ale_list_window_size = 10
let g:ale_python_pyls_config = {
\   'pyls': {
\     'plugins': {
\       'mccabe': {
\         'enabled': v:false
\       },
\       'pycodestyle': {
\         'enabled': v:false
\       },
\       'pylint': {
\         'enabled': v:false
\       }
\     }
\   },
\ }
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

" BufferDelete.
function! CommandCabbr(abbreviation, expansion)
  execute 'cabbr ' . a:abbreviation . ' <c-r>=getcmdpos() == 1 && getcmdtype() == ":" ? "' . a:expansion . '" : "' . a:abbreviation . '"<CR>'
endfunction
command! -nargs=+ CommandCabbr call CommandCabbr(<f-args>)

CommandCabbr bd BD
nnoremap <silent> <leader>d :BD<CR>

" Grep
set grepprg=rg\ --smart-case\ --vimgrep
autocmd QuickFixCmdPost *grep* cwindow

" Tagbar.
nnoremap <leader>to :TagbarToggle<CR>
nnoremap <leader>tp :TagbarTogglePause<CR>
let g:tagbar_left = 1
let g:tagbar_type_go = {
\   'ctagstype' : 'go',
\   'kinds'     : [
\     'p:package',
\     'i:imports:1',
\     'c:constants',
\     'v:variables',
\     't:types',
\     'n:interfaces',
\     'w:fields',
\     'e:embedded',
\     'm:methods',
\     'r:constructor',
\     'f:functions'
\   ],
\   'sro' : '.',
\   'kind2scope' : {
\     't' : 'ctype',
\     'n' : 'ntype'
\   },
\   'scope2kind' : {
\     'ctype' : 't',
\     'ntype' : 'n'
\   },
\   'ctagsbin'  : 'gotags',
\   'ctagsargs' : '-sort -silent'
\ }

" Asterisk.
map * <Plug>(asterisk-z*)
map # <Plug>(asterisk-z#)
map g* <Plug>(asterisk-gz*)
map g# <Plug>(asterisk-gz#)
let g:asterisk#keeppos = 1

" Easy-align.
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)
