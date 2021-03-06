{ config, pkgs, lib, ... }:

{
  nixpkgs.config = {
    allowUnsupportedSystem = true;
  };

  # fonts.fontconfig.enable = true;

  home = {
    extraOutputsToInstall = [ "man" ];
    language = {
      base = "en_US.UTF-8";
    };
    packages = [
      # util
      pkgs.awscli2
      pkgs.cachix
      # pkgs.cascadia-code
      pkgs.ctags
      pkgs.dust
      pkgs.fd
      pkgs.graphviz
      pkgs.hexyl
      # pkgs.inotify-tools
      pkgs.neofetch
      pkgs.ripgrep
      pkgs.tree
      pkgs.watch
      # pkgs.xclip

      # haskell
      pkgs.cabal-install
      pkgs.cabal2nix
      pkgs.nix-prefetch-git

      # go
      pkgs.golangci-lint
      pkgs.gopls
      pkgs.gotags
      pkgs.gotools

      # rust
      pkgs.cargo
      pkgs.rustc
    ];
    sessionVariables = {
      EDITOR = "vim";
      VISUAL = "vim";
    };
  };

  home.file.".config/kitty/kitty.conf" = {
    source = ../../.config/kitty/kitty.conf;
  };

  home.file.".ignore" = {
    source = ../../.ignore;
  };

  programs.bat = {
    enable = true;
  };

  programs.direnv = {
    enable = true;
    enableNixDirenvIntegration = true;
    stdlib = ''
      layout_virtualenv() {
        local venv_path="venv"
        source $''\{venv_path}/bin/activate
        unset PS1
      }
    '';
  };

  programs.exa = {
    enable = true;
    enableAliases = true;
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
      br = "branch";
      ci = "commit";
      co = "checkout";
      cp = "cherry-pick";
      st = "status";
    };
    attributes = [
      # https://gist.github.com/tekin/12500956bd56784728e490d8cef9cb81
      "*.c     diff=cpp"
      "*.h     diff=cpp"
      "*.c++   diff=cpp"
      "*.h++   diff=cpp"
      "*.cpp   diff=cpp"
      "*.hpp   diff=cpp"
      "*.cc    diff=cpp"
      "*.hh    diff=cpp"
      "*.m     diff=objc"
      "*.mm    diff=objc"
      "*.cs    diff=csharp"
      "*.css   diff=css"
      "*.html  diff=html"
      "*.xhtml diff=html"
      "*.ex    diff=elixir"
      "*.exs   diff=elixir"
      "*.go    diff=golang"
      "*.php   diff=php"
      "*.pl    diff=perl"
      "*.py    diff=python"
      "*.md    diff=markdown"
      "*.rb    diff=ruby"
      "*.rake  diff=ruby"
      "*.rs    diff=rust"
      "*.lisp  diff=lisp"
      "*.el    diff=lisp"
    ];
    delta = {
      enable = true;
    };
    extraConfig = {
      advice = { addIgnoredFile = false; };
      fetch = { prune = true; tags = true; };
      init = { templateDir = "~/.git-template"; };
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
    goPath = ".go/";
  };

  programs.home-manager = {
    enable = true;
  };

  # programs.htop = {
  #   enable = true;
  # };

  programs.jq = {
    enable = true;
  };

  programs.lesspipe = {
    enable = true;
  };

  programs.man = {
    enable = true;
    generateCaches = true;
  };

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    extraConfig = lib.strings.fileContents ../nvim/init.vim;
    plugins = [
      pkgs.vimPlugins.ale
      pkgs.vimPlugins.base16-vim
      pkgs.vimPlugins.bclose-vim
      pkgs.vimPlugins.fzf-vim
      pkgs.vimPlugins.fzfWrapper
      pkgs.vimPlugins.haskell-vim
      pkgs.vimPlugins.lightline-ale
      pkgs.vimPlugins.lightline-vim
      pkgs.vimPlugins.tagbar
      pkgs.vimPlugins.vim-abolish
      pkgs.vimPlugins.vim-asterisk
      pkgs.vimPlugins.vim-bufkill
      pkgs.vimPlugins.vim-commentary
      pkgs.vimPlugins.vim-easy-align
      pkgs.vimPlugins.vim-fugitive
      pkgs.vimPlugins.vim-polyglot
      pkgs.vimPlugins.vim-repeat
      pkgs.vimPlugins.vim-sensible
      pkgs.vimPlugins.vim-surround
      pkgs.vimPlugins.vim-textobj-comment
      pkgs.vimPlugins.vim-textobj-user
      pkgs.vimPlugins.vim-tmux-navigator
      pkgs.vimPlugins.vim-unimpaired
      pkgs.vimPlugins.vim-vinegar
    ];
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
      aws = {
        symbol = "";
      };
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
        format = "[$env_value]($style) ";
        style = "yellow";
        variable = "AWS_OKTA_PROFILE";
      };
      git_branch = {
        format = "[$symbol$branch]($style) ";
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
    plugins = with pkgs; [
      tmuxPlugins.copycat
      {
        plugin = tmuxPlugins.resurrect;
        extraConfig = "set -g @resurrect-strategy-nvim 'session'";
      }
      {
        plugin = tmuxPlugins.continuum;
        extraConfig = ''
          set -g @continuum-restore 'on'
          set -g @continuum-save-interval '60' # minutes
        '';
      }
      tmuxPlugins.vim-tmux-navigator
      tmuxPlugins.yank
    ];
    extraConfig = lib.strings.fileContents ../../.tmux.conf;
  };
  # tmux clipboardy things to maybe try to get working someday.
  # 'tmux-plugins/vim-tmux-focus-events'
  # 'roxma/vim-tmux-clipboard'

  # environment.pathsToLink = [ "/share/zsh" ] for completions
  programs.zsh = {
    enable = true;
    defaultKeymap = "viins";
    initExtra = ''
      if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then
        . ~/.nix-profile/etc/profile.d/nix.sh
      fi

      # Base16 Shell
      BASE16_SHELL="$HOME/.config/base16-shell/"
      [ -n "$PS1" ] && \
      [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
        eval "$("$BASE16_SHELL/profile_helper.sh")"
    '';
    profileExtra = ''
      if [ -f ~/.zlocal ]; then
        . ~/.zlocal
      fi
    '';
    shellAliases = {
      tm = "tmux a";
      cat = "bat";
    };
  };
}
