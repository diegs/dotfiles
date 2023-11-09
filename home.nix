{ config, lib, pkgs, pkgo, ... }:

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
      pkgo.wget
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
      pkgo.evans
      pkgs.go-migrate
      pkgs.graphite-cli
      # (pkgs.nodePackages.graphite-cli.override (_: {
      #   version = "0.22.15";
      #   src = pkgs.fetchurl {
      #     url = "https://registry.npmjs.org/@withgraphite/graphite-cli/-/graphite-cli-0.22.15.tgz";
      #     sha512 = "LshB8BhJrlLUhFG5H4gvpVca5R8p7UM8CSKVrIbYiRQ5y+9ASZ2st1zhITl0FwAQ6o4ZDN6vFK/1CCXy/OKPmw==";
      #   };
      # }))
      pkgs.python3Packages.grip
      pkgs.python3Packages.yq
      pkgs.openfortivpn

      # markdown
      pkgs.marksman

      # sysadmin
      pkgo.ansible
      pkgo.kubectl
      pkgs.k0sctl
      pkgs.damon
      pkgs.kubernetes-helm
      pkgs.k9s
      # pkgs-stable.awscli2
      pkgo.nomad
      pkgs.nomad-pack
      pkgo.rclone
      pkgo.sshpass
      pkgo.vault

      # data
      pkgo.kafkactl
      pkgo.mysql-client
      pkgo.postgresql
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
      # ".config/helix/themes".source = config/helix/themes;

      ".config/1Password/ssh/agent.toml".text = ''
        [[ssh-keys]]
        vault = "Private"

        [[ssh-keys]]
        vault = "DevOps"
      '';
    };

    sessionVariables = {
      # EDITOR = "emacsclient -c -nw -a ''";
      # VISUAL = "emacsclient -c -nw -a ''";
      SSH_AUTH_SOCK = "$HOME/Library/Group\ Containers/2BUA8C4S2C.com.1password/t/agent.sock";
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
      terminal-ansi16 = {
        src = pkgs.fetchFromGitHub {
          owner = "chtenb";
          repo = "ansi16";
          rev = "f8c8948008a5773a96bd736aa05cfff77fcfed71";
          sha256 = "sha256-tgu6wjaDFB/hCaoXkJHat0H7Ps3xNfK9Obb+3HxBGzA=";
        };
        file = "/terminal-ansi16.tmTheme";
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

  programs.eza = {
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
    enable = false;
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
    package = pkgs.jdk11;
  };

  programs.jq = {
    enable = true;
  };

  programs.jujutsu = {
    enable = true;
    settings = {
      ui = {
        default-command = "log";
      };
      user = {
        name = "Diego Pontoriero";
        email = "74719+diegs@users.noreply.github.com";
      };
    };
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

  programs.kakoune = {
    enable = true;
    config = {
        indentWidth = 2;
        numberLines = {
          enable = true;
          highlightCursor = true;
          relative = true;
        };
        scrollOff = {
          lines = 10;
        };
        showMatching = true;
        tabStop = 2;
        wrapLines = {
          enable = true;
          indent = true;
          marker = "⏎";
          word = true;
        };
        hooks = [
          {
            name = "KakBegin";
            option = ".*";
            commands = "eval %sh{kak-lsp --kakoune -s $kak_session}";
          }
          {
            name = "WinSetOption";
            option = "filetype=(rust|python|go|c|cpp|java|scala)";
            commands = ''
              lsp-enable-window
              lsp-auto-hover-enable
              lsp-auto-signature-help-enable
              set-option global lsp_auto_show_code_actions true
            '';
          }
          {
            name = "WinCreate";
            option = ".*";
            commands = ''
            	kakboard-enable
            	set-face global MenuForeground black,bright-blue
            	set-face global MenuBackground black,bright-white
            '';
          }
          {
            name = "WinSetOption";
            option = "filetype=go";
            commands = ''
              set-option window indentwidth 0
              hook window BufWritePre .* %{
                try %{ lsp-code-action-sync '^Organize Imports$' }
                lsp-formatting-sync
              }
            '';
          }
          {
            name = "BufCreate";
            option = "go\.(mod|sum)";
            commands = "set-option buffer indentwidth 0";
          }
          {
            name = "BufCreate";
            option = "Makefile";
            commands = "set-option buffer indentwidth 0";
          }
          {
            name = "WinSetOption";
            option = "filetype=rust";
            commands = "hook window BufWritePre .* lsp-formatting-sync";
          }
          {
            name = "InsertChar";
            option = "\\t";
            commands = "try %{ execute-keys -draft 'h<a-h><a-k>\A\h+\z<ret><a-;>;%opt{indentwidth}@' }";
          }
          {
            name = "InsertDelete";
            option = "' '";
            commands = "try %{ execute-keys -draft 'h<a-h><a-k>\A\h+\z<ret>i<space><esc><lt>' }";
          }
        ];
        keyMappings = [
          {
            mode = "normal";
            key = "<a-c>";
            effect = ":comment-line<ret>";
            docstring = "(un)comment selected lines using line comments";
          }
          {
            mode = "user";
            key = "l";
            effect = ":enter-user-mode lsp<ret>";
            docstring = "LSP mode";
          }
          {
            mode = "insert";
            key = "<tab>";
            effect = "<a-;>:try lsp-snippets-select-next-placeholders catch %{ execute-keys -with-hooks <lt>tab> }<ret>";
            docstring = "select next snippet placeholder";
          }
          {
            mode = "object";
            key = "a";
            effect = "<a-semicolon>lsp-object<ret>";
            docstring = "LSP any symbol";
          }
          {
            mode = "object";
            key = "<a-a>";
            effect = "<a-semicolon>lsp-object<ret>";
            docstring = "LSP any symbol";
          }
          {
            mode = "object";
            key = "e";
            effect = "<a-semicolon>lsp-object Function Method<ret>";
            docstring = "LSP function or method";
          }
          {
            mode = "object";
            key = "k";
            effect = "<a-semicolon>lsp-object Class Interface Struct<ret>";
            docstring = "LSP class interface or struct";
          }
          {
            mode = "object";
            key = "d";
            effect = "<a-semicolon>lsp-diagnostic-object --include-warnings<ret>";
            docstring = "LSP errors and warnings";
          }
          {
            mode = "object";
            key = "D";
            effect = "<a-semicolon>lsp-diagnostic-object<ret>";
            docstring = "LSP errors";
          }
          {
            mode = "window";
            key = "h";
            effect = ":nop %sh{wezterm cli activate-pane-direction Left}<ret>";
            docstring = "select pane left";
          }
          {
            mode = "window";
            key = "j";
            effect = ":nop %sh{wezterm cli activate-pane-direction Down}<ret>";
            docstring = "select pane down";
          }
          {
            mode = "window";
            key = "k";
            effect = ":nop %sh{wezterm cli activate-pane-direction Up}<ret>";
            docstring = "select pane up";
          }
          {
            mode = "window";
            key = "l";
            effect = ":nop %sh{wezterm cli activate-pane-direction Right}<ret>";
            docstring = "select pane right";
          }
          {
            mode = "window";
            key = "o";
            effect = ":nop %sh{wezterm cli activate-pane-direction Next}<ret>";
            docstring = "select pane next";
          }
          {
            mode = "window";
            key = "p";
            effect = ":nop %sh{wezterm cli activate-pane-direction Prev}<ret>";
            docstring = "select pane prev";
          }
          {
            mode = "window";
            key = "s";
            effect = ":new-below<ret>";
            docstring = "create horizontal pane";
          }
          {
            mode = "window";
            key = "v";
            effect = ":new-right<ret>";
            docstring = "create vertical pane";
          }
          {
            mode = "user";
            key = "w";
            effect = ":enter-user-mode window<ret>";
            docstring = "window mode";
          }
          {
            mode = "normal";
            key = "<c-w>";
            effect = ":enter-user-mode window<ret>";
            docstring = "window mode";
          }
          {
            mode = "normal";
            key = "<c-a>";
            effect = ":lsp-code-actions<ret>";
            docstring = "LSP code actions";
          }
        ];
    };
    defaultEditor = true;
    extraConfig = ''
    	define-command new-right -docstring "create a new kakoune client on the right" -params .. %{ wezterm-terminal-horizontal kak -c %val{session} -e "%arg{@}" }
    	alias global new-below new

    	define-command wezterm-terminal-vertical -params 1.. -docstring '
      wezterm-terminal-vertical <program> [<arguments>] [<arguments>]: create a new terminal as a wezterm pane
      The current pane is split into two, top and bottom
      The program passed as argument will be executed in the new terminal' \
      %{
        wezterm-terminal-impl split-pane --cwd "%val{client_env_PWD}" --pane-id "%val{client_env_WEZTERM_PANE}" --bottom -- %arg{@}
      }
      complete-command wezterm-terminal-vertical shell

      define-command wezterm-terminal-horizontal -params 1.. -docstring '
      wezterm-terminal-horizontal <program> [<arguments>]: create a new terminal as a wezterm pane
      The current pane is split into two, left and right
      The program passed as argument will be executed in the new terminal' \
      %{
          wezterm-terminal-impl split-pane --cwd "%val{client_env_PWD}" --pane-id "%val{client_env_WEZTERM_PANE}" --right -- %arg{@}
      }
      complete-command wezterm-terminal-horizontal shell
      
      alias global terminal wezterm-terminal-vertical
    '';
    plugins = [
        pkgs.kakounePlugins.kak-lsp
        pkgs.kakounePlugins.kakboard
    ];
  };

  programs.neovim = {
    enable = false;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    plugins = with pkgs.vimPlugins; [
       vim-polyglot
    ];
  };

  programs.emacs = {
    enable = false;
    package = (pkgs.emacsWithPackagesFromUsePackage {
      config = ./emacs/default.el;
      defaultInitFile = true;
      package = pkgs.emacs-unstable;
      alwaysEnsure = true;
      extraEmacsPackages = epkgs: [
        epkgs.treesit-grammars.with-all-grammars
      ];
    });
  };

  programs.ssh = {
    enable = true;
    forwardAgent = true;
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
      "*.smlxl.dev" = {
        user = "root";
      };
      "github.com" = {
        hostname = "ssh.github.com";
        port = 443;
      };
    };
  };

	programs.wezterm = {
  	enable = true;
  	extraConfig = builtins.readFile ./config/wezterm/wezterm.lua;
	};

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    defaultKeymap = "viins";
    enableVteIntegration = true;
    initExtra = ''
      # source ${pkgs.zsh-vi-mode}/share/zsh-vi-mode/zsh-vi-mode.plugin.zsh

      autoload -U promptinit; promptinit
      zstyle :prompt:pure:git:stash show yes
      zstyle :prompt:pure:prompt:success color green
      prompt pure

      autoload -U edit-command-line
      zle -N edit-command-line
      bindkey -M vicmd v edit-command-line

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
    shellAliases = {
      # e = "emacsclient -c -nw -a ''";
      kak = "wezterm cli spawn --cwd $PWD -- kak > /dev/null";
    };
    syntaxHighlighting = {
      enable = true;
    };
  };
}
