{ config, pkgs, lib, ... }:

{
  nixpkgs = {
    config = {
      allowUnsupportedSystem = true;
    };
    overlays = [ ];
  };

  home = {
    extraOutputsToInstall = [ "man" ];
    language = {
      base = "en_US.UTF-8";
    };
    packages = [
      # util
      # pkgs.awscli2
      pkgs.bottom
      pkgs.cachix
      pkgs.ctags
      pkgs.fd
      pkgs.graphviz
      pkgs.hexyl
      pkgs.procs
      pkgs.ripgrep
      pkgs.tree
      pkgs.watch

      # haskell
      # pkgs.cabal-install
      # pkgs.cabal2nix
      # pkgs.nix-prefetch-git

      # go
      # pkgs.golangci-lint
      # pkgs.gofumpt
      pkgs.gopls
      # pkgs.gotags
      pkgs.gotools

      # python
      pkgs.pyright

      # rust
      pkgs.cargo
      pkgs.rust-analyzer
      pkgs.rustc
      pkgs.rustfmt
    ];
    sessionVariables = {
      EDITOR = "hx";
      VISUAL = "hx";
    };
  };

  programs.bat = {
    enable = true;
  };

  programs.direnv = {
    enable = true;
    nix-direnv = {
      enable = true;
    };
    config = {
      load_direnv = false;
    };
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
      init = { defaultBranch = "main"; templateDir = "~/.git-template"; };
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
    goPath = "src/go";
    package = pkgs.go_1_17;
  };

  programs.helix  = {
    enable = true;
    settings = {
      theme = "monokai_pro_spectrum";
      editor = {
        cursor-shape = {
          insert = "bar";
          normal = "block";
          select = "underline";
        };
        line-number = "relative";
        lsp.display-messages = true;
        mouse = false;
      };
      keys.normal = {
        space.space = "file_picker";
      };
    };
  };

  programs.home-manager = {
    enable = true;
  };

  home.file.".ignore" = {
    source = ../../.ignore;
  };

  programs.java = {
    enable = true;
  };

  programs.jq = {
    enable = true;
  };

  home.file.".config/kitty/kitty.conf" = {
    source = ../kitty/kitty.conf;
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
      nodejs = {
        disabled = true;
      };
      python = {
        disabled = true;
      };
      rust = {
        disabled = true;
      };
    };
  };
  
  programs.tealdeer = {
    enable = true;
  };

  programs.tmux = {
    enable = true;
    baseIndex = 1;
    clock24 = true;
    escapeTime = 20;
    keyMode = "vi";
    newSession = true;
    terminal = "screen-256color";
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
      # Nix
      if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
      . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
      fi
      # End Nix

      if [ -f ~/.zlocal ]; then
        . ~/.zlocal
      fi
    '';
    shellAliases = {
      tm = "tmux a";
      cat = "bat";
      upgrade-nix = "sudo -i sh -c \"nix-channel --update && nix-env -iA nixpkgs.nix && launchctl remove org.nixos.nix-daemon && sleep 3 && launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist\"";
    };
  };
}
