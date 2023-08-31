{ config, pkgs, pkgo, ... }:

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
      pkgo.asciinema
      pkgo.asciinema-agg
      pkgo.graphviz
      pkgo.inetutils
      pkgo.tree
      pkgo.watch
      pkgs.pure-prompt
      pkgs.zk

      # rust alternates
      # pkgs.du-dust
      pkgs.fd
      pkgs.hexyl
      # pkgs.procs
      pkgs.ripgrep
      pkgs.sd

      # dev tools
      pkgs.bazelisk
      pkgs.buildah
      pkgs.cachix
      pkgs.cmake
      pkgs.go-migrate
      # pkgs.conan
      pkgs.nodePackages.graphite-cli
      pkgs.python3Packages.grip
      pkgs.python3Packages.yq

      # markdown
      pkgs.marksman

      # sysadmin
      pkgo.ansible
      pkgo.kubectl
      pkgs.k0sctl
      pkgs.damon
      pkgs.k9s
      # pkgs-stable.awscli2
      pkgo.nomad
      pkgs.nomad-pack
      pkgo.rclone
      pkgo.vault

      # data
      pkgo.kafkactl
      pkgo.mysql-client
      pkgo.redpanda
      
      # haskell
      # pkgs.cabal-install
      # pkgs.cabal2nix
      # pkgs.nix-prefetch-git
      
      # java
      (pkgo.gradle.override {
        javaToolchains = [ pkgs.jdk8 pkgs.jdk11 pkgs.jdk17 ];
      })
      pkgs.kotlin-language-server
      # pkgs.maven

      # scala
      # pkgs.ammonite
      # pkgs.metals
      # pkgs.scala_3
      # pkgs.scalafix

      # go
      pkgs.golangci-lint
      pkgs.gopls
      pkgs.gotools
      pkgs.grpcurl
      pkgs.grpcui
      
      # protobuf
      pkgs.buf
      pkgo.protobuf

      # python
      # pkgs.pyright

      # rust
      # pkgs.rustup
      pkgs.cargo
      pkgs.rust-analyzer
      pkgs.rustc
      pkgs.rustfmt

      # blockchain
      pkgs.nodePackages.ganache
      pkgs.solc

      # (pkgs.writeShellScriptBin "my-hello" ''
      #   echo "Hello, ${config.home.username}!"
      # '')
    ];

    file = {
      ".config/helix/themes".source = config/helix/themes;

      ".config/1Password/ssh/agent.toml".text = ''
        [[ssh-keys]]
        vault = "Private"

        [[ssh-keys]]
        vault = "DevOps"
      '';
    };
  
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
      advice = {
        addIgnoredFile = false;
      };
      commit = {
        gpgsign = true;
      };
      fetch = {
        prune = true;
        tags = true;
      };
      gpg = { 
        format = "ssh";
        ssh = {
          program = "/Applications/1Password.app/Contents/MacOS/op-ssh-sign";
        };
      };
      init = {
       defaultBranch = "main";
      };
      pull = {
        rebase = true;
      };
      push = { 
        autoSetupRemote = true;
        default = "current";
      };
      url = {
        "ssh://git@github.com/" = {
          insteadOf = "https://github.com/";
        };
      };
      user = { 
        signingkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILJasnFrDOljlqzQUCWT34ci8fp5/QgYh2QWvJM2l942";
      };
    };
    ignores = [
      ".direnv/"
      ".DS_Store"
    ];
  };

  programs.go = {
    enable = true;
    goPath = ".go";
    package = pkgs.go;
  };

  programs.helix = {
    enable = true;
    languages = {
      language = [
        {
          name = "java";
          indent = { tab-width = 4; unit = "    "; };
        }
        {
          name = "go";
          indent = { tab-width = 2; unit = "\t"; };
        }
      ];
    };
    settings = {
      theme = "ansi16";
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
        #statusline = {
        #  right = [ "diagnostics" "selections" "position" "file-encoding" ];
        #};
      };
    };
  };

  programs.home-manager.enable = true;

  programs.java = {
    enable = true;
    package = pkgs.jdk11_headless;
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
    changeDirWidgetCommand = "fd -H --type d";
    changeDirWidgetOptions = ["--height 100% --preview 'tree -C {} | head -200'"];
    defaultCommand = "fd -H --type f";
    fileWidgetCommand = "fd -H --type f";
    fileWidgetOptions = ["--height 100% --preview 'bat -f --style=numbers {}'"];
    historyWidgetOptions = [];
  };

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    plugins = with pkgs.vimPlugins; [
       vim-polyglot
    ];
  };

  programs.emacs = {
    enable = false;
    package = pkgs.emacsUnstable-nox;
    extraConfig = builtins.readFile ./default.el;
    extraPackages = epkgs: [
      epkgs.company
      # epkgs.eglot
      epkgs.meow
      epkgs.treesit-auto

      epkgs.go-mode
      epkgs.scala-mode
    ];
  };

  programs.ssh = {
    enable = true;
    extraOptionOverrides = {
      AddKeysToAgent = "yes";
      IdentityAgent = "'~/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock'";
      StrictHostKeyChecking = "no";
      UseKeychain = "yes";
    };
    matchBlocks = {
      "192.168.*" = {
        extraOptions = {
          HostKeyAlgorithms = "+ssh-rsa";
          PubkeyAcceptedKeyTypes = "+ssh-rsa";
          KexAlgorithms = "+diffie-hellman-group1-sha1";
        };
      };
      "172.17.*" = {
        extraOptions = {
          HostKeyAlgorithms = "+ssh-rsa";
          PubkeyAcceptedKeyTypes = "+ssh-rsa";
          KexAlgorithms = "+diffie-hellman-group1-sha1";
        };
      };
      "*.home.arpa" = {
        user = "admin";
        extraOptions = {
          HostKeyAlgorithms = "+ssh-rsa";
          PubkeyAcceptedKeyTypes = "+ssh-rsa";
          KexAlgorithms = "+diffie-hellman-group1-sha1";
        };
      };
      "*.node.consul" = {
        forwardAgent = true;
      };
      "github.com" = {
        hostname = "ssh.github.com";
        port = 443;
      };
    };
  };

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    defaultKeymap = "viins";
    enableVteIntegration = true;
    initExtra = ''
      # source ${pkgs.wezterm}/etc/profile.d/wezterm.sh
      # source ${pkgs.zsh-vi-mode}/share/zsh-vi-mode/zsh-vi-mode.plugin.zsh

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
    syntaxHighlighting = {
      enable = true;
    };
  };
}
