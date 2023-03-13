{ pkgs, pkgs-stable, ... }:
{
  home = {
    username = "diegs";
    homeDirectory = "/Users/diegs";
    stateVersion = "23.05";
    # language = { base = "en_US.UTF-8"; };
    # extraOutputsToInstall = [ "man" ];

    packages = [
      # util
      pkgs-stable.buildah
      pkgs-stable.graphviz
      pkgs-stable.tree
      pkgs-stable.watch

      # rust alternates
      # pkgs.delta
      pkgs.du-dust
      pkgs.fd
      pkgs.hexyl
      pkgs.procs
      pkgs.ripgrep
      pkgs.sd

      # dev tools
      pkgs.bazelisk
      pkgs.cachix
      pkgs.nodePackages.graphite-cli

      # c++
      # pkgs.clang-tools
      pkgs.cmake
      # pkgs.jemalloc
      # pkgs.gmp
      # pkgs.prometheus-cpp
      # pkgs.rdkafka

      # sysadmin
      pkgs.ansible
      pkgs-stable.awscli2
      pkgs.nomad
      pkgs.nomad-pack
      pkgs.vault

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
      theme = "terminal-ansi16";
    };
    themes = {
      terminal-ansi16 = builtins.readFile (pkgs.fetchFromGitHub {
        owner = "chtenb";
        repo = "ansi16";
        rev = "f8c8948008a5773a96bd736aa05cfff77fcfed71";
        sha256 = "sha256-tgu6wjaDFB/hCaoXkJHat0H7Ps3xNfK9Obb+3HxBGzA=";
      } + "/terminal-ansi16.tmTheme");
    };
  };

  programs.bottom = {
    enable = true;
    settings = {
      flags = {
        color = "default";  # default-light
      };
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
    delta = {
      enable = true;
      options = {
        navigate = true;
        syntax-theme = "terminal-ansi16";
        minus-style = "syntax 52";
        minus-emph-style = "syntax 88";
        plus-style = "syntax 22";
        plus-emph-style = "syntax 28";
      };
    };
    extraConfig = {
      advice = { addIgnoredFile = false; };
      fetch = { prune = true; tags = true; };
      init = { defaultBranch = "main"; };
      pull = { rebase = true; };
      push = { default = "current"; autoSetupRemote = true; };
      url."git@github.com:comitylabs".insteadOf = "https://github.com/comitylabs";
    };
    ignores = [
      ".envrc"
      ".mypy_cache/"
      "**/.settings/org.eclipse.*"
      "**/.idea"
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
      theme = "current";
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
          display-messages = false;
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

  home.file = {
    ".config/helix/update-theme.sh" = {
      executable = true;
      text = ''
        #!/usr/bin/env bash

        set -eu -o pipefail

        THEME=$(defaults read -g AppleInterfaceStyle || echo "Light")

        if [[ "$THEME" == "Dark" ]]; then
          ln -sf $''\{HOME}/.config/helix/themes/dark.toml $''\{HOME}/.config/helix/themes/current.toml
        else
          ln -sf $''\{HOME}/.config/helix/themes/light.toml $''\{HOME}/.config/helix/themes/current.toml
        fi

        pkill -SIGUSR1 hx
      '';
    };

    ".config/helix/themes/dark.toml" = {
      source = "${pkgs.helix}/lib/runtime/themes/catppuccin_macchiato.toml";
    };

    ".config/helix/themes/light.toml" = {
      source = "${pkgs.helix}/lib/runtime/themes/catppuccin_latte.toml";
    };
  };

  programs.home-manager = {
    enable = true;
  };

  programs.java = {
    enable = true;
    package = pkgs.jdk11_headless; 
  };

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

  programs.readline = {
    enable = true;
    variables = {
      editing-mode = "vi";
      show-all-if-ambiguous = true;
      page-completions = false;
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

  programs.wezterm = {
    enable = true;
    extraConfig = ''
      function scheme_for_appearance(appearance)
        os.execute(os.getenv("HOME") .. "/.config/helix/update-theme.sh")
        if appearance:find "Dark" then
          return "Catppuccin Macchiato"
        else
          return "Catppuccin Latte"
        end
      end

      return {
        font = wezterm.font("Berkeley Mono"),
        font_size = 14.0,
        color_scheme = scheme_for_appearance(wezterm.gui.get_appearance()),
        hide_tab_bar_if_only_one_tab = false,
        use_fancy_tab_bar = false,
        tab_bar_at_bottom = true,
        audible_bell = "Disabled",
        keys = {
          {key="n", mods="SHIFT|CTRL", action="ToggleFullScreen"},
        }
      }
    '';
  };

  programs.zsh = {
    enable = true;
    defaultKeymap = "viins";
    enableSyntaxHighlighting = true;
    enableVteIntegration = false;
    history = {
      expireDuplicatesFirst = true;
      save = 100000;
    };
    initExtra = ''
      autoload -U edit-command-line
      zle -N edit-command-line
      bindkey -M vicmd v edit-command-line
    '';
    profileExtra = ''
      # Local
      if [ -f ~/.zlocal ]; then
        . ~/.zlocal
      fi
    '';
    shellAliases = {
      ssh = "TERM=xterm-256color ssh";
    };
  };
}
