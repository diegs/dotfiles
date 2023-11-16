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

      (pkgs.writeShellScriptBin "kakw" ''
        set -euo pipefail
        kak "''${@:2}"
        wezterm cli activate-pane --pane-id $1
      '')
    ];

    file = {
      ".config/1Password/ssh/agent.toml".text = ''
        [[ssh-keys]]
        vault = "Private"

        [[ssh-keys]]
        vault = "DevOps"
      '';

      ".editrc".text = "bind -v";
    };

    sessionVariables = {
      SSH_AUTH_SOCK = "$HOME/Library/Group\ Containers/2BUA8C4S2C.com.1password/t/agent.sock";
    };
  };

  programs.atuin = {
    enable = true;
    settings = {
      auto_sync = false;
      filter_mode_shell_up_key_binding = "session";
      search_mode = "fulltext";
      show_preview = true;
      style = "compact";
      secrets_filter = false;
      update_check = false;
    };
  };

  programs.bat = {
    enable = true;
    config = {
      theme = "ansi";
    };
  };

  programs.dircolors = {
    enable = true;
  };

  programs.direnv = {
    enable = true;
    nix-direnv = {
      enable = true;
    };
    # config = {
    #   load_direnv = false;
    # };
    # stdlib = ''
    #   layout_virtualenv() {
    #     local venv_path="venv"
    #     source ''${venv_path}/bin/activate
    #     unset PS1
    #   }
    # '';
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
        syntax-theme = "ansi";
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
            '';
              # set-face global MenuForeground white,cyan
              # set-face global MenuBackground bright-white,cyan
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
            commands = ''try %{ execute-keys -draft "h<a-h><a-k>\A\h+\z<ret><a-;>;%opt{indentwidth}@" }'';
          }
          {
            name = "InsertDelete";
            option = "' '";
            commands = ''try %{ execute-keys -draft "h<a-h><a-k>\A\h+\z<ret>i<space><esc><lt>" }'';
          }
          {
            name = "InsertCompletionShow";
            option = ".*";
            commands = ''
              try %{
                  # this command temporarily removes cursors preceded by whitespace;
                  # if there are no cursors left, it raises an error, does not
                  # continue to execute the mapping commands, and the error is eaten
                  # by the `try` command so no warning appears.
                  execute-keys -draft 'h<a-K>\h<ret>'
                  map window insert <tab> <c-o>
                  hook -once -always window InsertCompletionHide .* %{
                    unmap window insert <tab> <c-o>
                  }
              }
            '';
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
    changeDirWidgetCommand = "fd -H --type d --color=always";
    changeDirWidgetOptions = ["--ansi" "--height 100%" "--preview 'tree -C {} | head -200'"];
    defaultCommand = "fd -H --type f --color=always";
    defaultOptions = ["--ansi"];
    fileWidgetCommand = "fd -H --type f --color=always";
    fileWidgetOptions = ["--ansi" "--height 100%" "--preview 'bat -f --style=numbers {}'"];
    historyWidgetOptions = [];
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
    extraConfig = ''
      function get_appearance()
        if wezterm.gui then
          return wezterm.gui.get_appearance()
        end
        return 'Dark'
      end

      function scheme_for_appearance(appearance)
        if appearance:find 'Dark' then
          return 'Spacegray Eighties (Gogh)'
        else
          return 'Terminal Basic'
          -- return 'Borland'
        end
      end

      return {
        color_scheme = scheme_for_appearance(get_appearance()),
        font = wezterm.font 'SF Mono',
        font_size = 14.0,
        use_fancy_tab_bar = false,
        hide_tab_bar_if_only_one_tab = false,
        tab_bar_at_bottom = true,
        switch_to_last_active_tab_when_closing_tab = true,
        tab_max_width = 32,
        quit_when_all_windows_are_closed = false,
        audible_bell = "Disabled",
        initial_rows = 48,
        initial_cols = 140,
        use_resize_increments = true,
        leader = { key = "W", mods = "SHIFT|CMD" },
        keys = {
          { key = "{", mods = "SHIFT|CTRL|CMD", action = wezterm.action.MoveTabRelative(-1) },
          { key = "}", mods = "SHIFT|CTRL|CMD", action = wezterm.action.MoveTabRelative(1) },
          { key = "s", mods = "LEADER", action = wezterm.action.SplitVertical },
          { key = "v", mods = "LEADER", action = wezterm.action.SplitHorizontal },
          { key = "z", mods = "LEADER", action = wezterm.action.TogglePaneZoomState },
          { key = "h", mods = "LEADER", action = wezterm.action.ActivatePaneDirection("Left") },
          { key = "j", mods = "LEADER", action = wezterm.action.ActivatePaneDirection("Down") },
          { key = "k", mods = "LEADER", action = wezterm.action.ActivatePaneDirection("Up") },
          { key = "l", mods = "LEADER", action = wezterm.action.ActivatePaneDirection("Right") },
          { key = "h", mods = "LEADER|SHIFT", action = wezterm.action.AdjustPaneSize { 'Left', 5 } },
          { key = "j", mods = "LEADER|SHIFT", action = wezterm.action.AdjustPaneSize { 'Down', 5 } },
          { key = "k", mods = "LEADER|SHIFT", action = wezterm.action.AdjustPaneSize { 'Up', 5 } },
          { key = "l", mods = "LEADER|SHIFT", action = wezterm.action.AdjustPaneSize { 'Right', 5 } },
        },
      }
    '';
  };

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    defaultKeymap = "viins";
    enableVteIntegration = false;
    initExtra = ''
      autoload -U promptinit; promptinit
      zstyle :prompt:pure:git:stash show yes
      prompt pure

      autoload -U edit-command-line
      zle -N edit-command-line
      bindkey -M vicmd v edit-command-line

      _zsh_autosuggest_strategy_atuin() {
        suggestion=$(atuin search --limit 1 --search-mode prefix --filter-mode global --cmd-only $1)
      }

      ZSH_AUTOSUGGEST_STRATEGY=(completion)
      bindkey -M viins '^O' autosuggest-accept
      bindkey -M viins '^[[Z' reverse-menu-complete
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
      cat = "bat";
      kak = "wezterm cli spawn --cwd $PWD -- kakw $WEZTERM_PANE > /dev/null";
    };
    syntaxHighlighting = {
      enable = true;
    };
  };
}
