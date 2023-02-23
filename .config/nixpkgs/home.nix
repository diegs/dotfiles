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
      pkgs.bottom
      pkgs.delta
      pkgs.du-dust
      pkgs.fd
      pkgs.nodePackages.graphite-cli
      pkgs.graphviz
      pkgs.hexyl
      pkgs.procs
      pkgs.ripgrep
      pkgs.sd
      pkgs.tree
      pkgs.watch

      # tooling
      pkgs.bazelisk
      pkgs.cachix

      # c++
      # pkgs.clang-tools
      # pkgs.cmake
      # pkgs.jemalloc
      # pkgs.gmp
      # pkgs.prometheus-cpp
      # pkgs.rdkafka

      # sysadmin
      pkgs.ansible
      pkgs.awscli2
      pkgs.nomad
      pkgs.nomad-pack

      # data
      pkgs.kafkactl
      pkgs.mysql-client
      pkgs.redpanda
      
      # haskell
      # pkgs.cabal-install
      # pkgs.cabal2nix
      # pkgs.nix-prefetch-git
      
      # java
      pkgs.jdt-language-server
      pkgs.gradle
      pkgs.maven

      # scala
      pkgs.scala
      pkgs.metals

      # go
      pkgs.golangci-lint
      pkgs.gopls
      pkgs.gotools
      pkgs.grpcurl
      pkgs.grpcui
      
      # protobuf
      pkgs.buf
      pkgs.protobuf

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
    config = {
      theme = "ansi";
    };
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
    goPath = ".go";
    package = pkgs.go;
  };

  programs.helix  = {
    enable = true;
    languages = [
      {
        name = "java";
        indent = { tab-width = 2; unit = "  "; };
        language-server = {
          # command = "/Users/diegs/src/java-language-server/dist/lang_server_mac.sh";
          command = "jdt-language-server";
          args = [
            "-configuration" "/Users/diegs/.cache/jdtls/config"
            "-data" "/Users/diegs/.cache/jdtls/workspace"
          ];
        };
      }
      {
        name = "go";
        indent = { tab-width = 2; unit = "\t"; };
      }
    ];
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
        lsp = {
          display-messages = true;
        };
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
    enable = true;
    package = pkgs.jdk11_headless; 
    # package = pkgs.jdk_headless; 
  };

  programs.jq = {
    enable = true;
  };

  programs.kitty = {
    enable = true;
    font = {
      name = "Berkeley Mono";
      size = 13;
    };
    keybindings = {
      "cmd+t" = "new_tab_with_cwd";
      "cmd+ctrl+shift+[" = "move_tab_backward";
      "cmd+ctrl+shift+]" = "move_tab_forward";
    };
    settings = {
      disable_ligatures = "cursor";
      macos_option_as_alt = "left";
      enable_audio_bell = false;
      term = "xterm-256color";
      resize_in_steps = true;
      tab_title_template = "\" {fmt.fg.red}{bell_symbol}{activity_symbol}{fmt.fg.tab}{title} \"";
      tab_separator = "\" \"";
      tab_bar_style = "separator";
      active_tab_foreground = "#fafafa";
      active_tab_background = "#0184bc";
      active_tab_font_style = "bold";
      inactive_tab_foreground = "#383a42";
      inactive_tab_background = "#dadada";
      inactive_tab_font_style = "normal";
    };
    extraConfig = builtins.readFile ../kitty/themes/edge-light.theme.conf;
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
        disabled = true;
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
      java = {
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
    enableSyntaxHighlighting = true;
    enableVteIntegration = true;
    initExtra = ''
      if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then
        . ~/.nix-profile/etc/profile.d/nix.sh
      fi
    '';
    profileExtra = ''
      if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
      . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
      fi

      # Local
      if [ -f ~/.zlocal ]; then
        . ~/.zlocal
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
      upgrade-nix = "sudo -i sh -c \"nix-channel --update && nix-env -u && launchctl remove org.nixos.nix-daemon && sleep 3 && launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist\"";
    };
  };
}
