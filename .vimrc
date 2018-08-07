if &compatible
 set nocompatible
endif

let mapleader = ','

call plug#begin('~/.vim/plugged')

" Navigation.
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'
Plug 'majutsushi/tagbar'

" Visual.
Plug 'chriskempson/base16-vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Completion.
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'prabirshrestha/asyncomplete-lsp.vim'

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
Plug 'kana/vim-textobj-user'
Plug 'glts/vim-textobj-comment'

" Integrations.
Plug 'christoomey/vim-tmux-navigator'
Plug 'tmux-plugins/vim-tmux-focus-events'
Plug 'janko-m/vim-test'
Plug 'tpope/vim-fugitive'

call plug#end()

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
set spell
highlight SpellBad cterm=undercurl ctermbg=18 gui=undercurl guisp=#F07178
highlight clear SpellCap

" FZF.
let g:fzf_command_prefix = 'Fzf'
let g:fzf_buffers_jump = 1
nnoremap <silent> <C-p> :FzfFiles<CR>
nnoremap <silent> <C-b> :FzfBuffers<CR>
command! -bang -nargs=* Rg
      \ call fzf#vim#grep(
      \   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
      \   <bang>0 ? fzf#vim#with_preview('up:60%')
      \           : fzf#vim#with_preview('right:50%:hidden', '?'),
      \   <bang>0)
nnoremap <silent> <leader>r :Rg<CR>

" LSP.
nnoremap <silent> <leader>h :LspHover<CR>
nnoremap <silent> <leader>g :LspDefinition<CR>
set completeopt-=preview

if executable('go-langserver')
  au User lsp_setup call lsp#register_server({
        \ 'name': 'go-langserver',
        \ 'cmd': {server_info->['go-langserver', '-mode', 'stdio', '-gocodecompletion']},
        \ 'whitelist': ['go'],
        \ })
endif

if executable('pyls')
  au User lsp_setup call lsp#register_server({
        \ 'name': 'pyls',
        \ 'cmd': {server_info->['pyls']},
        \ 'whitelist': ['python'],
        \ })
endif

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
nnoremap <leader>t :TagbarToggle<CR>
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
