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
      # pkgs.glibcLocales
      pkgs.hexyl
      pkgs.inotify-tools
      pkgs.graphviz
      pkgs.ripgrep
      # pkgs.s-tui
      pkgs.xclip
      pkgs.tree

      # go
      pkgs.golangci-lint
      pkgs.gopls
      pkgs.gotags
      pkgs.gotools

      # haskell
      pkgs.cabal-install
      pkgs.cabal2nix
      pkgs.nix-prefetch-git
    ];
    sessionVariables = {
      EDITOR = "vim";
      # LOCALES_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
      VISUAL = "vim";
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

  programs.command-not-found = {
    enable = true;
  };

  programs.direnv = {
    enable = true;
  };

  programs.emacs = {
    enable = false;
    package = pkgs.emacs-nox;
    extraPackages = epkgs: [
      # epkgs.better-defaults
      epkgs.base16-theme
      epkgs.company
      epkgs.counsel
      epkgs.evil
      epkgs.flycheck
      epkgs.go-mode
      epkgs.goto-chg
      epkgs.ivy
      epkgs.ivy-hydra
      epkgs.lsp-mode
      epkgs.magit
      epkgs.nix-mode
      epkgs.projectile
      epkgs.undo-tree
      epkgs.yaml-mode
    ];
  };
  home.file.".emacs.d" = {
    source = ../../.emacs.d;
  };

  programs.feh = {
    enable = true;
  };

  programs.fzf = {
    enable = true;
    changeDirWidgetCommand = "fd --type d";
    defaultCommand = "fd --type f";
    fileWidgetCommand = "fd --type f";
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

  programs.home-manager = {
    enable = true;
  };

  programs.htop = {
    enable = true;
  };

  programs.jq = {
    enable = true;
  };

  programs.lesspipe = {
    enable = true;
  };

  programs.man = {
    enable = true;
  };

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    extraConfig = lib.strings.fileContents ../nvim/init.vim;
    plugins = [
      customPlugins.salt-vim
      customPlugins.vim-asterisk
      customPlugins.vim-bufkill
      customPlugins.vim-textobj-comment
      customPlugins.vim-textobj-user
      pkgs.vimPlugins.ale
      pkgs.vimPlugins.base16-vim
      pkgs.vimPlugins.fzf-vim
      pkgs.vimPlugins.fzfWrapper
      pkgs.vimPlugins.goyo-vim
      pkgs.vimPlugins.haskell-vim
      pkgs.vimPlugins.lightline-ale
      pkgs.vimPlugins.lightline-vim
      pkgs.vimPlugins.tagbar
      pkgs.vimPlugins.vim-abolish
      pkgs.vimPlugins.vim-commentary
      pkgs.vimPlugins.vim-easy-align
      pkgs.vimPlugins.vim-fugitive
      pkgs.vimPlugins.vim-orgmode
      pkgs.vimPlugins.vim-polyglot
      pkgs.vimPlugins.vim-repeat
      pkgs.vimPlugins.vim-sensible
      pkgs.vimPlugins.vim-surround
      pkgs.vimPlugins.vim-tmux-navigator
      pkgs.vimPlugins.vim-unimpaired
      pkgs.vimPlugins.vim-vinegar
    ];
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
  # tmux clipboardy things to maybe try to get working someday.
  # 'tmux-plugins/vim-tmux-focus-events'
  # 'roxma/vim-tmux-clipboard'

  xdg.userDirs = {
    enable = true;
    desktop = "\"$HOME/.desktop\"";
    documents = "\"$HOME\"";
    download = "\"$HOME/downloads\"";
    music = "\"$HOME/music\"";
    pictures = "\"$HOME\"";
    publicShare = "\"$HOME\"";
    templates = "\"$HOME\"";
    videos = "\"$HOME\"";
  };
}
