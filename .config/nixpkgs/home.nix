{ config, pkgs, lib, ... }:

{
  nixpkgs = {
    config = {
      allowUnfree = true;
      allowUnsupportedSystem = true;
    };
    overlays = [ ];
  };

  manual.manpages.enable = true;

  home = {
    extraOutputsToInstall = [ "man" ];
    language = { base = "en_US.UTF-8"; };
    stateVersion = "18.09";
    packages = [
      # util
      # pkgs.awscli2
      pkgs.bottom
      pkgs.cachix
      pkgs.ctags
      pkgs.delta
      pkgs.du-dust
      pkgs.fd
      pkgs.graphviz
      pkgs.hexyl
      pkgs.procs
      pkgs.ripgrep
      pkgs.sd
      pkgs.spr
      pkgs.tree
      pkgs.watch
      
      # haskell
      # pkgs.cabal-install
      # pkgs.cabal2nix
      # pkgs.nix-prefetch-git
      
      # kafka
      pkgs.kafkactl
      # pkgs.redpanda

      # go
      pkgs.golangci-lint
      pkgs.gopls
      pkgs.gotools
      
      # protobuf
      pkgs.buf

      # python
      pkgs.pyright

      # rust
      # pkgs.rustup
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
    changeDirWidgetOptions = ["--preview 'tree -C {} | head -200'"];
    defaultCommand = "fd --type f";
    fileWidgetCommand = "fd --type f";
    fileWidgetOptions = ["--preview 'bat -f --style=numbers {}'"];
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
      l = "!f() {\n git log --ext-diff $@ \n}; f";
      sh = "!f() {\n git show --ext-diff $@ \n}; f";
    };
    delta = {
      enable = true;
      options = {
        features = "decorations";
        light = true;
      };
    };
    extraConfig = {
      advice = { addIgnoredFile = false; };
      fetch = { prune = true; tags = true; };
      init = { defaultBranch = "main"; };
      pull = { rebase = true; };
      push = { default = "current"; autoSetupRemote = true; };
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
    package = pkgs.go_1_19;
  };

  programs.helix  = {
    enable = true;
    settings = {
      theme = "edge_light";
      editor = {
        color-modes = true;
        cursorline = true;
        cursor-shape = {
          insert = "bar";
          normal = "block";
          select = "underline";
        };
        file-picker = {
          hidden = false;
        };
        indent-guides = {
          render = true;
        };
        line-number = "relative";
        mouse = false;
        whitespace = {
          render = {
            space = "all";
            tab = "all";
            newline = "none";
          };
        };
      };
      keys.normal = {
        # space.space = "file_picker";
        ";" = "repeat_last_motion";
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
    enable = false;
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
      buf = {
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

  programs.zsh = {
    enable = true;
    defaultKeymap = "viins";
    dotDir = ".config/zsh";
    enableSyntaxHighlighting = true;
    enableVteIntegration = true;
    initExtra = ''
      if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then
        . ~/.nix-profile/etc/profile.d/nix.sh
      fi
      
      # Local
      if [ -f ~/.zshrc_local ]; then
        . ~/.zshrc_local
      fi
    '';
    profileExtra = ''
      # Nix
      if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
      . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
      fi
      # End Nix

      # Homebrew
      if [ -e '/opt/homebrew/bin/brew' ]; then
        eval "$(/opt/homebrew/bin/brew shellenv)"
      fi

      # Local
      if [ -f ~/.zprofile_local ]; then
        . ~/.zprofile_local
      fi
    '';
    sessionVariables = {
      # DFT_DISPLAY = "side-by-side-show-both";
      # DFT_TAB_WIDTH = 2;
    };
    shellAliases = {
      cat = "bat";
      du = "dust";
      diff = "delta";
      ps = "procs";
      upgrade-nix = "sudo -i sh -c \"nix-channel --update && nix-env -u && launchctl remove org.nixos.nix-daemon && sleep 3 && launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist\"";
    };
  };
}
