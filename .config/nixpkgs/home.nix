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
  targets.genericLinux.enable = true;
  home = {
    extraOutputsToInstall = [ "man" ];
    packages = [
      # util
      pkgs.awscli
      pkgs.ctags
      pkgs.fd
      pkgs.exa
      pkgs.cascadia-code
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
      ls = "exa";
      ll = "ls -alF";
      la = "ls -aa";
      l = "ls -F";
      tm = "tmux a";
      cat = "bat";
      colors = ''for i in {0..255}; do printf "\x1b[38;5;$''\{i}mcolor%-5i\x1b[0m" $i ; if ! (( ($i + 1 ) % 8 )); then echo ; fi ; done'';
    };
    profileExtra = ''
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
      ".envrc"
      ".mypy_cache/"
    ];
  };

  programs.go = {
    enable = true;
    goPath = ".";
  };

  xdg.userDirs = {
    enable = true;
    desktop = "$HOME/";
    documents = "$HOME/";
    download = "$HOME/downloads";
    music = "$HOME/";
    pictures = "$HOME/";
    publicShare = "$HOME/";
    templates = "$HOME/";
    videos = "$HOME/";
  };

  # home.file.".config/nvim" = {
  #   source = ../nvim;
  # };

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    extraConfig = lib.strings.fileContents ../nvim/init.vim;
    plugins = [
      pkgs.vimPlugins.ale
      pkgs.vimPlugins.base16-vim
      pkgs.vimPlugins.fzf-vim
      pkgs.vimPlugins.fzfWrapper
      pkgs.vimPlugins.goyo-vim
      pkgs.vimPlugins.haskell-vim
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
    extraConfig = lib.strings.fileContents ../../.tmux.conf;
  };
}
