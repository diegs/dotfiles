{ config, pkgs, lib, ... }:

let
  customPlugins.salt-vim = pkgs.vimUtils.buildVimPlugin {
    name = "salt-vim";
    src = pkgs.fetchFromGitHub {
      owner = "saltstack";
      repo = "salt-vim";
      rev = "6ca9e3500cc39dd417b411435d58a1b720b331cc";
      sha256 = "0r79bpl98xcsmkw6dg83cf1ghn89rzsr011zirk3v1wfxclri2c4";
    };
  };
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
  aspy-refactor-imports = pkgs.python37.pkgs.buildPythonPackage rec {
    pname = "aspy.refactor-imports";
    version = "1.1.0";

    src = pkgs.python37.pkgs.fetchPypi {
      inherit version;
      pname = "aspy.refactor_imports";
      sha256 = "0m8x725xswsxh25xb2ypnbl6xwp6vill8l1n72fhpmd0s6dqshc8";
    };

    buildInputs = [
      pkgs.python37.pkgs.cached-property
    ];

    doCheck = false;

    meta = {
      homepage = "https://github.com/asottile/reorder_python_imports";
      description = "Rewrites source to reorder python imports";
    };
  };
  reorder-python-imports = pkgs.python37.pkgs.buildPythonPackage rec {
    pname = "reorder-python-imports";
    version = "1.6.1";

    src = pkgs.python37.pkgs.fetchPypi {
      inherit version;
      pname = "reorder_python_imports";
      sha256 = "1c04d11c07d4c48d75b9a7f0ab5db4b61be42d294fd902c1efd2b1e0bc146d22";
    };

    propagatedBuildInputs = [
      aspy-refactor-imports
      pkgs.python37.pkgs.cached-property
    ];

    doCheck = false;

    meta = {
      homepage = "https://github.com/asottile/reorder_python_imports";
      description = "Rewrites source to reorder python imports";
    };
  };
  tokenize-rt = pkgs.python37.pkgs.buildPythonPackage rec {
    pname = "tokenize-rt";
    version = "3.2.0";

    src = pkgs.python37.pkgs.fetchPypi {
      inherit version;
      pname = "tokenize_rt";
      sha256 = "1krkx6xlfw3sxab8h80d09pgiyi1a5wl40f50f52y410yvlfwi1g";
    };

    # buildInputs = [
    #   pkgs.python37.pkgs.tokenize-rt
    # ];

    doCheck = false;

    meta = {
      homepage = "https://github.com/asottile/tokenize-rt";
      description = "A wrapper around the stdlib `tokenize` which roundtrips.";
    };
  };
  pyupgrade = pkgs.python37.pkgs.buildPythonApplication rec {
    pname = "pyupgrade";
    version = "1.23.0";

    src = pkgs.python37.pkgs.fetchPypi {
      inherit version;
      pname = "pyupgrade";
      sha256 = "1vqpm63fnc35zdvfck6yr4ss9vvqzxkxqsyk31l2fgyb0k1a3xyd";
    };

    propagatedBuildInputs = [
      tokenize-rt
    ];

    doCheck = false;

    meta = {
      homepage = "https://github.com/asottile/pyupgrade";
      description = "A tool (and pre-commit hook) to automatically upgrade syntax for newer versions of the language.";
    };
  };
  pre-commit = pkgs.python37.pkgs.buildPythonPackage rec {
    pname = "pre-commit";
    version = "1.18.1";

    src = pkgs.python37.pkgs.fetchPypi {
      pname = "pre_commit";
      inherit version;
      sha256 = "1762f2a551732e250d0e16131d3bf9e653adb6ec262e58dfe033906750503235";
    };

    propagatedBuildInputs = [
      pkgs.python37.pkgs.aspy-yaml
      pkgs.python37.pkgs.cached-property
      pkgs.python37.pkgs.cfgv
      pkgs.python37.pkgs.identify
      pkgs.python37.pkgs.nodeenv
      pkgs.python37.pkgs.six
      pkgs.python37.pkgs.toml
      pkgs.python37.pkgs.virtualenvwrapper
      pkgs.python37.pkgs.importlib-metadata
    ];

    doCheck = false;

    meta = {
      homepage = "https://pre-commit.com";
      description = "A framework for managing and maintaining multi-language pre-commit hooks.";
    };
  };

  # tmux clipboardy things to maybe try to get working someday.
  # 'tmux-plugins/vim-tmux-focus-events'
  # 'roxma/vim-tmux-clipboard'

in {
  home = {
    extraOutputsToInstall = [ "man" ];
    packages = [
      pkgs.awscli
      pkgs.ctags
      pkgs.fd
      pkgs.exa
      pkgs.fira-code
      pkgs.fira-code-symbols
      pkgs.hexyl
      pkgs.inotify-tools
      pkgs.graphviz
      pkgs.ripgrep
      pkgs.tree

      # go
      pkgs.glide
      pkgs.goimports
      pkgs.golangci-lint
      pkgs.gotags

      # haskell
      pkgs.cabal-install
      pkgs.cabal2nix
      pkgs.nix-prefetch-git

      # pyupgrade

      # # python
      # (
      #   pkgs.python37.withPackages (python-packages: with python-packages; [
      #   black
      #   mypy
      #   # pre-commit
      #   pyls-black
      #   pyls-mypy
      #   python-language-server
      #   reorder-python-imports
      #   virtualenvwrapper
      # ]))
    ];
    sessionVariables = {
      EDITOR = "vim";
      VISUAL = "vim";
    };
  };

  programs.command-not-found = {
    enable = true;
  };

  programs.direnv = {
    enable = true;
  };

  programs.home-manager = {
    enable = true;
  };

  programs.man = {
    enable = false;
  };

  programs.jq = {
    enable = true;
  };

  programs.htop = {
    enable = true;
  };

  programs.lesspipe = {
    enable = true;
  };

  programs.fzf = {
    enable = true;
    changeDirWidgetCommand = "fd --type d";
    defaultCommand = "fd --type f";
    fileWidgetCommand = "fd --type f";
  };

  programs.pazi = {
    enable = true;
  };

  programs.readline = {
    enable = true;
    variables = {
      editing-mode = "vi";
    };
  };

  programs.bash = {
    enable = true;
    shellAliases = {
      cdl = "cd ~/src/github.com/lyft";
      ls = "exa";
      ll = "ls -alF";
      la = "ls -aa";
      l = "ls -F";
      tm = "tmux a";
      cat = "bat";
      colors = ''for i in {0..255}; do printf "\x1b[38;5;$''\{i}mcolor%-5i\x1b[0m" $i ; if ! (( ($i + 1 ) % 8 )); then echo ; fi ; done'';
    };
    profileExtra = ''
      if [ -f ~/.nix-profile/etc/profile.d/nix.sh ]; then
        . ~/.nix-profile/etc/profile.d/nix.sh
      fi

      export NIX_PATH="$HOME/.nix-defexpr/channels$''\{NIX_PATH:+:}$NIX_PATH";
      export PATH=~/bin:$PATH

      if [ -f ~/.bash_local ]; then
        . ~/.bash_local
      fi
    '';
    initExtra = lib.mkBefore ''
      # Base16 Shell
      BASE16_SHELL="$HOME/.config/base16-shell/"
      [ -n "$PS1" ] && \
      [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
        eval "$("$BASE16_SHELL/profile_helper.sh")"
    '';
  };

  programs.bat = {
    enable = true;
  };

  programs.git = {
    enable = true;
    userName = "Diego Pontoriero";
    userEmail = "74719+diegs@users.noreply.github.com";
    aliases = {
      co = "checkout";
      br = "branch";
      ci = "commit";
      st = "status";
    };
    extraConfig = {
      fetch = { prune = true; tags = true; };
      pull = { rebase = true; };
      push = { default = "current"; };
      url."git@github.com:".insteadOf = "https://github.com/";
    };
    ignores = [
      ".mypy_cache/"
    ];
  };

  programs.go = {
    enable = true;
    goPath = ".";
  };

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    extraConfig = ''
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
      if filereadable(expand("~/.vimrc_background"))
        let base16colorspace=256
        source ~/.vimrc_background
      endif
      highlight SpellBad cterm=undercurl ctermbg=238 gui=undercurl guisp=#F07178
      highlight Comment ctermfg=gray
      highlight clear SpellCap

      " Lightline
      let g:lightline = {
      \  'colorscheme': 'wombat',
      \  'active': {
      \    'left': [ [ 'mode', 'paste' ],
      \              [ 'gitbranch', 'readonly', 'filename', 'modified' ] ],
      \    'right': [ [ 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_ok' ],
      \               [ 'lineinfo' ],
      \               [ 'percent' ],
      \               [ 'fileformat', 'fileencoding', 'filetype' ] ],
      \  },
      \  'component': {
      \    'filename': '%f',
      \  },
      \  'component_function': {
      \    'gitbranch': 'fugitive#head',
      \  },
      \  'component_expand': {
      \    'linter_checking': 'lightline#ale#checking',
      \    'linter_warnings': 'lightline#ale#warnings',
      \    'linter_errors': 'lightline#ale#errors',
      \    'linter_ok': 'lightline#ale#ok',
      \  },
      \  'component_type': {
      \   'linter_checking': 'left',
      \   'linter_warnings': 'warning',
      \   'linter_errors': 'error',
      \   'linter_ok': 'left',
      \ },
      \}

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
      let g:ale_fix_on_save = 1
      let g:ale_lint_on_text_changed = 'normal'
      let g:ale_lint_on_insert_leave = 1
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
    plugins = [
      pkgs.vimPlugins.ale
      pkgs.vimPlugins.base16-vim
      pkgs.vimPlugins.fzf-vim
      pkgs.vimPlugins.fzfWrapper
      pkgs.vimPlugins.goyo-vim
      pkgs.vimPlugins.lightline-ale
      pkgs.vimPlugins.lightline-vim
      customPlugins.salt-vim
      pkgs.vimPlugins.tagbar
      pkgs.vimPlugins.vim-abolish
      customPlugins.vim-asterisk
      customPlugins.vim-bufkill
      pkgs.vimPlugins.vim-commentary
      pkgs.vimPlugins.vim-easy-align
      pkgs.vimPlugins.vim-fugitive
      pkgs.vimPlugins.vim-polyglot
      pkgs.vimPlugins.vim-repeat
      pkgs.vimPlugins.vim-sensible
      pkgs.vimPlugins.vim-surround
      customPlugins.vim-textobj-comment
      customPlugins.vim-textobj-user
      pkgs.vimPlugins.vim-tmux-navigator
      pkgs.vimPlugins.vim-unimpaired
      pkgs.vimPlugins.vim-vinegar
    ];
  };

  programs.starship = {
    enable = true;
    settings = {
      battery = {
        disabled = true;
      };
      cmd_duration = {
        disabled = true;
      };
      directory = {
        style = "bold blue";
        truncate_to_repo = false;
        truncation_length = 20;
      };
      env_var = {
        prefix = "";
        style = "yellow";
        variable = "AWS_OKTA_PROFILE";
      };
      git_branch = {
        prefix = "";
        symbol = "";
      };
      golang = {
        disabled = true;
      };
      python = {
        disabled = true;
      };
    };
  };

  programs.tmux = {
    enable = true;
    baseIndex = 1;
    clock24 = true;
    escapeTime = 20;
    keyMode = "vi";
    newSession = true;
    terminal = "screen-256color";
    plugins = [
      pkgs.tmuxPlugins.copycat
      pkgs.tmuxPlugins.vim-tmux-navigator
      pkgs.tmuxPlugins.yank
    ];
    extraConfig = ''
      unbind C-b
      set -g prefix `
      bind ` send-prefix

      bind c new-window -c "#{pane_current_path}"
      bind '"' split-window -c "#{pane_current_path}"
      bind % split-window -h -c "#{pane_current_path}"

      bind m previous-window

      # set -g set-clipboard on
      # set-option -g set-titles on
      # set-option -g set-titles-string "#I #W"
      # set-option -ga terminal-overrides ",xterm-256color:Tc"

      set -g visual-activity off
      set -g visual-bell off
      set -g visual-silence off
      setw -g monitor-activity off
      set -g bell-action none

      setw -g clock-mode-colour colour5
      setw -g mode-style "fg=colour1 bg=colour18 bold"

      set -g pane-border-style "fg=colour19 bg=colour0"
      set -g pane-active-border-style "bg=colour0 fg=colour9"

      set -g status-position bottom
      set -g status-justify left
      set -g status-style "bg=colour240 fg=colour15"

      set -g status-left ""
      set -g status-right "#[fg=colour233,bg=colour19] %m/%d #[fg=colour233,bg=colour8] %H:%M "
      set -g status-right-length 50
      set -g status-left-length 20

      setw -g window-status-current-style "fg=colour1 bg=colour4"
      setw -g window-status-current-format " #I#[fg=colour249]:#[fg=colour255]#W#[fg=colour249]#F "

      setw -g window-status-style "fg=colour9 bg=colour240"
      setw -g window-status-format " #I#[fg=colour237]:#[fg=colour250]#W#[fg=colour244]#F "

      setw -g window-status-bell-style "fg=colour255 bg=colour1 bold"
      set -g message-style "fg=colour232 bg=colour16 bold"
    '';
  };
}
