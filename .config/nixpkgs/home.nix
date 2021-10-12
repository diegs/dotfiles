{ config, pkgs, lib, ... }:

{
  nixpkgs = {
    config = {
      allowUnsupportedSystem = true;
    };
    overlays = [
      (self: super:
        let
          pkgs_x86_64 = import <nixpkgs> { localSystem = "x86_64-darwin"; overlays = [
            (self: super:
              let
                lib = super.lib;
              in
                rec {
                  python39 = super.python39.override {
                    packageOverrides = self: super: {
                      beautifulsoup4 = super.beautifulsoup4.overrideAttrs (old: {
                        propagatedBuildInputs = lib.remove super.lxml old.propagatedBuildInputs;
                      });
                    };
                  };
                  python39Packages = python39.pkgs;
                }
            ) ]; };
        in {
          kitty = pkgs_x86_64.kitty;
	      }
      )
    ];
  };

  home = {
    extraOutputsToInstall = [ "man" ];
    language = {
      base = "en_US.UTF-8";
    };
    packages = [
      # util
      pkgs.awscli2
      pkgs.bottom
      pkgs.cachix
      pkgs.ctags
      # pkgs.fast-cli
      pkgs.fd
      pkgs.graphviz
      pkgs.hexyl
      pkgs.procs
      pkgs.ripgrep
      pkgs.tree
      pkgs.watch

      # fonts
      pkgs.cascadia-code

      # haskell
      pkgs.cabal-install
      pkgs.cabal2nix
      pkgs.nix-prefetch-git

      # go
      pkgs.golangci-lint
      pkgs.gofumpt
      pkgs.gopls
      pkgs.gotags
      pkgs.gotools

      # python
      pkgs.pyright

      # rust
      pkgs.cargo
      pkgs.rust-analyzer
      pkgs.rustc
    ];
    sessionVariables = {
      ALTERNATE_EDITOR = "";
      EDITOR = "emacsclient -t";
      VISUAL = "emacsclient -c -a emacs";
    };
  };

  home.file.".ignore" = {
    source = ../../.ignore;
  };

  programs.bat = {
    enable = true;
  };

  programs.direnv = {
    enable = true;
    nix-direnv = {
      enable = true;
    };
    stdlib = ''
      layout_virtualenv() {
        local venv_path="venv"
        source $''\{venv_path}/bin/activate
        unset PS1
      }
    '';
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-nox;
    extraPackages = epkgs: [
      # Core.
      epkgs.base16-theme
      epkgs.evil
      epkgs.undo-tree

      # Navigation.
      epkgs.consult
      epkgs.marginalia
      epkgs.orderless
      epkgs.selectrum
      epkgs.selectrum-prescient

      # Coding.
      epkgs.company
      epkgs.eglot
      epkgs.yasnippet

      # Modes.
      # epkgs.tree-sitter
      # epkgs.tree-sitter-langs
      epkgs.go-mode
      epkgs.nix-mode
      epkgs.rust-mode
      epkgs.salt-mode
      epkgs.yaml-mode

      # Utils.
      epkgs.magit
    ];
  };
  home.file.".emacs" = {
    source = ../../.emacs;
  };

  programs.exa = {
    enable = true;
    enableAliases = true;
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

  programs.jq = {
    enable = true;
  };

  programs.kitty = {
    enable = true;
    font = {
      name = "Cascadia Code Regular";
      size = 14;
    };
    settings = {
      macos_option_as_alt = "left";
    };
  };

  programs.lesspipe = {
    enable = true;
  };

  programs.man = {
    enable = true;
    generateCaches = true;
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
      tmuxPlugins.vim-tmux-navigator
    ];
    extraConfig = lib.strings.fileContents ../../.tmux.conf;
  };

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
      e = "emacsclient -c -a=''";
      tm = "tmux a";
      cat = "bat";
    };
  };
}
