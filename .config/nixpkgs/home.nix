{ config, pkgs, ... }:

{
  home.packages = [
    pkgs.awscli
    pkgs.ctags
    pkgs.fzf
    pkgs.jq
    pkgs.git
    pkgs.htop
    pkgs.ripgrep
    pkgs.tmux
    pkgs.tree

    # pkgs.gitAndTools.pre-commit

    # go
    pkgs.glide
    pkgs.go
    pkgs.goimports
    pkgs.gotags

    # haskell
    pkgs.cabal-install
    pkgs.cabal2nix
    pkgs.nix-prefetch-git

    # python
    pkgs.python37Packages.black
    pkgs.python37Packages.pyls-black
    pkgs.python37Packages.pyls-mypy
    pkgs.python37Packages.python-language-server
  ];

  programs.home-manager.enable = true;

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    configure = {
      customRC = ''
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
        highlight SpellBad cterm=undercurl ctermbg=238 gui=undercurl guisp=#F07178
        highlight Comment ctermfg=gray
        " highlight clear SpellCap

        " FZF.
        let g:fzf_command_prefix = 'Fzf'
        let g:fzf_buffers_jump = 1
        nnoremap <silent> <C-p> :FzfFiles<CR>
        nnoremap <silent> <C-b> :FzfBuffers<CR>
        nnoremap <silent> <leader>r :FzfRg<CR>

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
        \  'python': ['black'],
        \  'rust': ['rustfmt'],
        \}
        let g:ale_fix_on_save = 1
        let g:ale_lint_on_text_changed = 'normal'
        let g:ale_lint_on_insert_leave = 1
        let g:airline#extensions#ale#enabled = 1
        let g:ale_completion_enabled = 1
        let g:ale_go_gofmt_options = '-s'
        let g:ale_go_golangci_lint_options = '--fast -c ~/.golangci.yml '
        let g:ale_go_golangci_lint_package = 1
        let g:ale_python_pyls_config = {
        \   'pyls': {
        \     'plugins': {
        \       'pycodestyle': {
        \         'enabled': v:false
        \       },
        \       'mccabe': {
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

        " " Asterisk.
        " map * <Plug>(asterisk-z*)
        " map # <Plug>(asterisk-z#)
        " map g* <Plug>(asterisk-gz*)
        " map g# <Plug>(asterisk-gz#)
        " let g:asterisk#keeppos = 1
        "
        " autocmd BufNewFile,BufRead ~/src/github.com/lyft/dispatch/* let b:ale_fixers = {'python': ['black']}
        " autocmd BufNewFile,BufRead ~/src/github.com/lyft/marketstate/* let b:ale_fixers = {'python': ['black']}
        "
        " let g:ale_javascript_prettier_options = '--no-bracket-spacing'
        " let g:ale_rust_rls_toolchain = 'stable'
        " let g:ale_fix_on_save = 1
        " let g:ale_set_balloons = 1
        " let g:ale_set_highlights = 0
        " let g:ale_sign_column_always = 1
        "
        " " Git
        " au FileType gitcommit au! BufEnter COMMIT_EDITMSG call setpos('.', [0, 1, 1, 0])
        "
        "
        " " Easy-align.
        " xmap ga <Plug>(EasyAlign)
        " nmap ga <Plug>(EasyAlign)
      '';
      plug.plugins = with pkgs.vimPlugins; [
        ale
        vim-airline
        vim-airline-themes
        base16-vim
        fzfWrapper
        fzf-vim
        polyglot
        tagbar
        vim-commentary
        vim-fugitive
        vim-sensible
        vim-tmux-navigator
        vim-vinegar

        # 'vim-bufkill'
        # 'haya14busa/vim-asterisk'
        # 'tpope/vim-repeat'
        # 'tpope/vim-abolish'
        # 'tpope/vim-surround'
        # 'tpope/vim-unimpaired'
        # 'junegunn/vim-easy-align'
        # 'kana/vim-textobj-user'
        # 'glts/vim-textobj-comment'
        # 'tmux-plugins/vim-tmux-focus-events'
        # 'roxma/vim-tmux-clipboard'
      ];
    };
  };
}
