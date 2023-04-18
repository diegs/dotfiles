{ pkgs, pkgs-stable, ... }:
let
  username = "diegs";
  homeDir = "/Users/${username}";
in {
  home = {
    username = username;
    homeDirectory = homeDir;
    stateVersion = "23.05";

    packages = [
      # util
      pkgs.asciinema
      pkgs.asciinema-agg
      pkgs.buildah
      pkgs-stable.graphviz
      pkgs.pure-prompt
      pkgs-stable.tree
      pkgs-stable.watch
      pkgs.zk

      # rust alternates
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
      pkgs.python3Packages.grip

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

      # blockchain
      pkgs.nodePackages.ganache
      pkgs.solc
    ];
    sessionVariables = {
      EDITOR = "hx";
      VISUAL = "hx";
    };
  };

  programs.atuin = {
    enable = true;
    settings = {
      history_filter = [
        "^cd "
        "^ls$"
        "^ll$"
      ];
      filter_mode_shell_up_key_binding = "session";
      search_mode = "skim";
      show_preview = true;
      style = "compact";
      update_check = false;
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
        color = "default";
      };
    };
  };

  programs.broot = {
    enable = true;
    settings = {
      modal = true;
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

  programs.git = {
    enable = true;
    userName = "Diego Pontoriero";
    userEmail = "74719+diegs@users.noreply.github.com";
    delta = {
      enable = true;
      options = {
        navigate = true;
        syntax-theme = "terminal-ansi16";
        minus-style = "reverse red";
        minus-emph-style = "reverse bold red";
        plus-style = "reverse green";
        plus-emph-style = "reverse bold green";
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
            "-configuration" "${homeDir}/.cache/jdtls/config"
            "-data" "${homeDir}/.cache/jdtls/workspace"
          ];
        };
      }
      {
        name = "go";
        indent = { tab-width = 2; unit = "\t"; };
      }
    ];
    settings = {
      theme = "wezterm";
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
        soft-wrap = {
          enable = true;
        };
        statusline = {
          right = [ "version-control" "diagnostics" "selections" "position" "file-encoding" ];
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

        # DARK_THEME_PATH="${pkgs.helix}/lib/runtime/themes/catppuccin_macchiato.toml"
        # LIGHT_THEME_PATH="${pkgs.helix}/lib/runtime/themes/catppuccin_latte.toml"
        DARK_THEME_PATH="${homeDir}/.config/helix/extra-themes/edge_default.toml"
        LIGHT_THEME_PATH="${homeDir}/.config/helix/extra-themes/edge_light.toml"

        THEME=$(defaults read -g AppleInterfaceStyle || echo "Light")

        if [[ "$THEME" == "Dark" ]]; then
          ln -sf $''\{DARK_THEME_PATH} $''\{HOME}/.config/helix/themes/wezterm.toml
        else
          ln -sf $''\{LIGHT_THEME_PATH} $''\{HOME}/.config/helix/themes/wezterm.toml
        fi

        pkill -SIGUSR1 hx
      '';
    };

    ".config/helix/extra-themes" = {
      source = ./helix/themes;
    };

    # ".config/helix/themes/dark.toml" = {
    #   source = "${pkgs.helix}/lib/runtime/themes/catppuccin_macchiato.toml";
    # };

    # ".config/helix/themes/light.toml" = {
    #   source = "${pkgs.helix}/lib/runtime/themes/catppuccin_latte.toml";
    # };
  };

  programs.home-manager = {
    enable = true;
  };

  programs.java = {
    enable = true;
    package = pkgs.jdk_headless; 
  };

  programs.jq = {
    enable = true;
  };

  programs.readline = {
    enable = true;
    variables = {
      editing-mode = "vi";
      show-all-if-ambiguous = true;
      page-completions = false;
    };
  };

  programs.skim = {
    enable = true;
    changeDirWidgetCommand = "fd --type d";
    changeDirWidgetOptions = ["--preview 'tree -C {} | head -200'"];
    defaultCommand = "fd --type f";
    fileWidgetCommand = "fd --type f";
    fileWidgetOptions = ["--preview 'bat -f --style=numbers {}'"];
    historyWidgetOptions = [];
  };

  programs.wezterm = {
    enable = true;
    extraConfig = ''
      function scheme_for_appearance(appearance)
        os.execute(os.getenv("HOME") .. "/.config/helix/update-theme.sh")
        if appearance:find "Dark" then
          return "Edge Dark (base16)"
          -- return "Catppuccin Macchiato"
        else
          return "Edge Light (base16)"
          -- return "Catppuccin Latte"
        end
      end

      return {
        font = wezterm.font("Berkeley Mono"),
        font_size = 14.0,
        color_scheme = scheme_for_appearance(wezterm.gui.get_appearance()),
        quit_when_all_windows_are_closed = false,
        hide_tab_bar_if_only_one_tab = false,
        use_fancy_tab_bar = false,
        tab_bar_at_bottom = true,
        audible_bell = "Disabled",
        initial_rows = 48,
        initial_cols = 140,
        keys = {
          { key= "{", mods = "SHIFT|CTRL|CMD", action = wezterm.action.MoveTabRelative(-1) },
          { key= "}", mods = "SHIFT|CTRL|CMD", action = wezterm.action.MoveTabRelative(1) },
        }
      }
    '';
  };

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    defaultKeymap = "viins";
    enableSyntaxHighlighting = true;
    # enableVteIntegration = false;
    # history = {
    #   expireDuplicatesFirst = true;
    #   save = 100000;
    # };
    initExtra = ''
      source ${pkgs.wezterm}/etc/profile.d/wezterm.sh

      autoload -U promptinit; promptinit
      zstyle :prompt:pure:git:stash show yes
      zstyle :prompt:pure:prompt:success color green
      prompt pure

      # Re-enable once hx can handle this
      # autoload -U edit-command-line
      # zle -N edit-command-line
      # bindkey -M vicmd v edit-command-line

      _zsh_autosuggest_strategy_atuin() {
        suggestion=$(atuin search --limit 1 --search-mode prefix --filter-mode global --cmd-only $1)
      }

      ZSH_AUTOSUGGEST_STRATEGY=(completion)
      bindkey '^E' autosuggest-accept
    '';
    profileExtra = ''
      # Nix
      if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
          . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
      fi
      # End Nix

      # Local
      if [ -f ~/.zlocal ]; then
        . ~/.zlocal
      fi
    '';
  };
}
