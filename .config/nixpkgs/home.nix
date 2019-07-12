{ config, pkgs, ... }:

let
  customPlugins.vim-asterisk = pkgs.vimUtils.buildVimPlugin {
    name = "vim-asterisk";
    src = pkgs.fetchFromGitHub {
      owner = "haya14busa";
      repo = "vim-asterisk";
      rev = "1a805e320aea9d671be129b4162ea905a8bc095e";
      sha256 = "1dl4lsrvz7n476ajry4bmmby8f09qa66nnasxv5jypgajn5w73zh";
    };
  };
  customPlugins.vim-bufkill = pkgs.vimUtils.buildVimPlugin {
    name = "vim-bufkill";
    src = pkgs.fetchFromGitHub {
      owner = "qpkorr";
      repo = "vim-bufkill";
      rev = "795dd38f3cff69d0d8fe9e71847907e200860959";
      sha256 = "1nji86vjjbfjw4xy52yazq53hrlsr7v30xkx2awgiakz7ih0bdxa";
    };
  };
  customPlugins.vim-textobj-user = pkgs.vimUtils.buildVimPlugin {
    name = "vim-textobj-user";
    src = pkgs.fetchFromGitHub {
      owner = "kana";
      repo = "vim-textobj-user";
      rev = "074ce2575543f790290b189860597a3dcac1f79d";
      sha256 = "15wnqkxjjksgn8a7d3lkbf8d97r4w159bajrcf1adpxw8hhli1vc";
    };
  };
  customPlugins.vim-textobj-comment = pkgs.vimUtils.buildVimPlugin {
    name = "vim-textobj-comment";
    configurePhase = ''
      rm Makefile
    '';
    src = pkgs.fetchFromGitHub {
      owner = "glts";
      repo = "vim-textobj-comment";
      rev = "58ae4571b76a5bf74850698f23d235eef991dd4b";
      sha256 = "00wc14chwjfx95gl3yzbxm1ajx88zpzqz0ckl7xvd7gvkrf0mx04";
    };
  };
  # tmux clipboardy things to maybe try to get working someday.
  # 'tmux-plugins/vim-tmux-focus-events'
  # 'roxma/vim-tmux-clipboard'

in {
  home.packages = [
    pkgs.awscli
    pkgs.ctags
    pkgs.fzf
    pkgs.jq
    pkgs.git
    pkgs.exa
    pkgs.htop
    pkgs.inotify-tools
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
    pkgs.python37
    pkgs.python37Packages.black
    pkgs.python37Packages.mypy
    pkgs.python37Packages.pyls-black
    pkgs.python37Packages.pyls-mypy
    pkgs.python37Packages.python-language-server
  ];

  programs.home-manager.enable = true;
  programs.man.enable = false;
  home.extraOutputsToInstall = [ "man" ];

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
        set clipboard=unnamed

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
        highlight clear SpellCap

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
        " autocmd BufNewFile,BufRead ~/src/github.com/something/* let b:ale_fixers = {'python': []}
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
      '';
      packages.myVimPackage = with pkgs.vimPlugins // customPlugins; {
        start = [
          ale
          base16-vim
          fzf-vim
          fzfWrapper
          tagbar
          vim-abolish
          vim-airline
          vim-airline-themes
          vim-asterisk
          vim-bufkill
          vim-commentary
          vim-easy-align
          vim-fugitive
          vim-polyglot
          vim-repeat
          vim-sensible
          vim-surround
          vim-textobj-comment
          vim-textobj-user
          vim-tmux-navigator
          vim-unimpaired
          vim-vinegar
        ];
        opt = [];
      };
    };
  };
}
