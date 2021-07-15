lua require('init')

" Lightline.
let g:lightline = {
\ 'colorscheme': 'wombat',
\ 'active': {
\   'left':  [ [ 'mode', 'paste' ],
\              [ 'gitbranch', 'readonly', 'filename', 'modified' ] ],
\   'right': [ [ 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_infos', 'linter_ok' ],
\              [ 'lineinfo' ],
\              [ 'percent' ],
\              [ 'fileformat', 'fileencoding', 'filetype'] ],
\ },
\ 'component_function': {
\   'filename': 'LightlineFilename',
\   'gitbranch': 'FugitiveHead'
\ },
\ 'component_expand': {
\   'linter_checking': 'lightline#ale#checking',
\   'linter_infos': 'lightline#ale#infos',
\   'linter_warnings': 'lightline#ale#warnings',
\   'linter_errors': 'lightline#ale#errors',
\   'linter_ok': 'lightline#ale#ok',
\ },
\ 'component_type': {
\   'linter_checking': 'right',
\   'linter_infos': 'right',
\   'linter_warnings': 'warning',
\   'linter_errors': 'error',
\   'linter_ok': 'right',
\ },
\}

function! LightlineFilename()
  let root = fnamemodify(get(b:, 'git_dir'), ':h')
  let path = expand('%:p')
  if path[:len(root)-1] ==# root
    return path[len(root)+1:]
  endif
  return expand('%')
endfunction

" Colorscheme.
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

" FZF.
let g:fzf_command_prefix = 'Fzf'
" let g:fzf_buffers_jump = 1
nnoremap <silent> <C-p> :FzfFiles<CR>
nnoremap <silent> <C-b> :FzfBuffers<CR>
nnoremap <silent> <leader>r :FzfRg<CR>
autocmd! FileType fzf
autocmd  FileType fzf set laststatus=0 noshowmode noruler
  \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler

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

