{ config, lib, pkgs, pkgo, ... }:

let
  username = "diegs";
  homeDir = "/Users/${username}";
in {
  home = {
    username = username;
    homeDirectory = homeDir;
    stateVersion = "23.11";

    packages = [
      # util
      pkgo.asciinema
      pkgo.asciinema-agg
      pkgo.graphviz
      pkgo.inetutils
      pkgo.tree
      pkgo.watch
      # (pkgs.fishPlugins.pure.overrideAttrs(o: {
      #   doCheck = false;
      # }))
      pkgo.wget
      pkgs.zk

      # (pkgs.writeShellScriptBin "kak-kitty-tab" ''
      #   kak "''${@:2}"
      #   kitten @ focus-tab --match=id:$1
      # '')
      (pkgs.writeShellScriptBin "fzf" ''
        SK_DEFAULT_OPTS="$FZF_DEFAULT_OPTS" sk "$@"
      '')

      # rust alternates
      # pkgs.du-dust
      pkgs.fd
      pkgs.hexyl
      # pkgs.procs
      pkgs.ripgrep
      pkgs.sd

      # dev tools
      pkgs.bazelisk
      # pkgo.buildah
      pkgs.cachix
      pkgs.cmake
      pkgs.go-migrate
      pkgs.graphite-cli
      # (pkgs.nodePackages.graphite-cli.override (_: {
      #   version = "1.1.4";
      #   src = pkgs.fetchurl {
      #     url = "https://registry.npmjs.org/@withgraphite/graphite-cli/-/graphite-cli-1.1.4.tgz";
      #     # sha512 = "sha512-LshB8BhJrlLUhFG5H4gvpVca5R8p7UM8CSKVrIbYiRQ5y+9ASZ2st1zhITl0FwAQ6o4ZDN6vFK/1CCXy/OKPmw==";
      #     sha512 = lib.fakeSha512;
      #   };
      # }))
      pkgo.python3Packages.grip
      # pkgs.python3Packages.yq
      pkgs.yq-go
      pkgs.openfortivpn
      pkgs.colima
      pkgs.docker-client
      pkgs.docker-buildx
      pkgs.age
      pkgs.sops

      # markdown
      pkgs.marksman

      # sysadmin
      pkgo.ansible
      pkgs.cloudflared
      pkgs.kubectl
      pkgs.damon
      # pkgs.kubeseal
      # pkgs.fluxcd
      # pkgs.weave-gitops
      pkgs.kubeconform
      pkgs.kustomize
      pkgs.kubernetes-helm
      pkgs.k9s
      # pkgs-stable.awscli2
      pkgo.nomad
      pkgo.nomad-pack
      pkgo.rclone
      pkgo.sshpass
      pkgs.vault

      # data
      pkgo.kafkactl
      pkgo.mysql-client
      pkgo.postgresql
      pkgo.redpanda
      pkgs.duckdb

      # haskell
      # pkgs.cabal-install
      # pkgs.cabal2nix
      # pkgs.nix-prefetch-git

      # java
      (pkgo.gradle.override {
        javaToolchains = [ pkgs.jdk8 pkgs.jdk11 pkgs.jdk17 ];
      })
      pkgo.maven
      pkgs.kotlin-language-server
      pkgs.java-language-server

      # scala
      # pkgs.ammonite
      (pkgs.metals.override {
        jre = pkgs.jdk17;
      })
      (pkgs.sbt.override {
        jre = pkgs.jdk17;
      })
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
      pkgs.protobuf_26

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
    ];

    file = {
      ".config/1Password/ssh/agent.toml".text = ''
        [[ssh-keys]]
        vault = "Private"

        [[ssh-keys]]
        vault = "DevOps"
      '';

      ".config/kak-lsp/kak-lsp.toml".source = config/kak-lsp/kak-lsp.toml;
      ".hushlogin".text = "";
      ".ignore".text = ".git/";
    };

    sessionVariables = {
      XDG_DATA_DIRS = "$HOME/.nix-profile/share:$XDG_DATA_DIRS";
      SSH_AUTH_SOCK = "$HOME/Library/Group\ Containers/2BUA8C4S2C.com.1password/t/agent.sock";
    };
  };

  programs = {
    alacritty = {
      enable = true;
      settings = {
        import = [
            "~/.config/alacritty/themes/themes/alabaster.toml"
        ];
        window = {
          resize_increments = true;
          option_as_alt = "OnlyLeft";
        };
        font = {
          normal = { family = "Monaspace Neon"; style = "Regular"; };
          size = 14;
        };
      };
    };

    atuin = {
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

    bat = {
      enable = true;
      config = {
        theme = "ansi";
      };
    };

    dircolors = {
      enable = true;
    };

    direnv = {
      enable = true;
      nix-direnv = {
        enable = true;
      };
    };

    eza = {
      enable = true;
    };

    fish = {
      enable = true;
      functions = {
        # fish_title = ''
        #   # emacs' "term" is basically the only term that can't handle it.
        #   if not set -q INSIDE_EMACS; or string match -vq '*,term:*' -- $INSIDE_EMACS
        #       # If we're connected via ssh, we print the hostname.
        #       set -l ssh
        #       set -q SSH_TTY
        #       and set ssh "["(prompt_hostname | string sub -l 10 | string collect)"]"
        #       # An override for the current command is passed as the first parameter.
        #       # This is used by `fg` to show the true process name, among others.
        #       if set -q argv[1]
        #           echo -- $ssh (string sub -l 20 -- $argv[1]) (prompt_pwd -d 1 -D 1)
        #       else
        #           # Don't print "fish" because it's redundant
        #           set -l command (status current-command)
        #           if test "$command" = fish
        #               set command
        #           end
        #           echo -- $ssh (string sub -l 20 -- $command) (prompt_pwd -d 1 -D 1)
        #       end
        #   end
        # '';
        fish_title = {
          argumentNames = "last_command";
          description = "Set title to current folder and shell name";
          body = ''
            set --local prompt
            if test -z "$last_command"
              set prompt (fish_prompt_pwd_dir_length=$pure_shorten_window_title_current_directory_length prompt_pwd)
            else
              set prompt (status current-command 2>/dev/null; or echo $_)
            end
            echo $prompt
          '';
        };
        gitignore = "curl -sL https://www.gitignore.io/api/$argv";
      };
      plugins = [
        {
          name = "fish-async-prompt";
          src = pkgs.fetchFromGitHub {
            owner = "acomagu";
            repo = "fish-async-prompt";
            rev = "v1.2.0";
            sha256 = "sha256-B7Ze0a5Zp+5JVsQUOv97mKHh5wiv3ejsDhJMrK7YOx4=";
          };
        }
        {
          name = "pure";
          src = pkgs.fetchFromGitHub {
            owner = "pure-fish";
            repo = "pure";
            rev = "v4.8.1";
            sha256 = "sha256-MnlqKRmMNVp6g9tet8sr5Vd8LmJAbZqLIGoDE5rlu8E=";
          };
        }
      ];
      shellAliases = {
        cat = "bat";
        ssh = "TERM=xterm-256color /usr/bin/ssh";
        # kakw = "kitten @ launch --type tab --cwd current --location after --no-response --title kak --copy-env kak-kitty-tab $KITTY_WINDOW_ID";
        # lm = "ln -sf ~/.config/kitty/tango_light.conf ~/.config/kitty/current-theme.conf && pkill -USR1 -a kitty";
        # dm = "ln -sf ~/.config/kitty/space_gray_eighties.conf ~/.config/kitty/current-theme.conf && pkill -USR1 -a kitty";
        k = "kubectl";
        kg = "kubectl get";
        kns = "kubectl config set-context --current --namespace";
      };
      shellInit = ''
        # Nix
        source '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish'
        # End Nix
        set --prepend fish_complete_path /Users/diegs/.nix-profile/share/fish/completions
        set --prepend fish_complete_path /Users/diegs/.nix-profile/share/fish/vendor_completions.d
        source ~/.local.fish
      '';
      interactiveShellInit = ''
        set -g async_prompt_functions _pure_prompt_git
        set -g pure_shorten_window_title_current_directory_length 1
      '';
    };

    git = {
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

    go = {
      enable = true;
      goPath = ".go";
      package = pkgs.go;
    };

    home-manager.enable = true;

    java = {
      enable = true;
      package = pkgs.jdk17;
    };

    jq = {
      enable = true;
    };

    jujutsu = {
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

    kakoune = {
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
          ui = {
            enableMouse = false;
          };
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
              name = "KakEnd";
              option = ".*";
              commands = "eval %sh{pkill -f 'kak-lsp -s $kak_session'}";
            }
            {
              name = "WinSetOption";
              option = "filetype=(rust|python|go|c|cpp|java|scala)";
              commands = ''
                lsp-enable-window
                lsp-auto-signature-help-enable
                set-option global lsp_auto_show_code_actions true
              '';
                # lsp-auto-hover-enable
            }
            # {
            #   name = "WinCreate";
            #   option = ".*";
            #   commands = ''
            #     kakboard-enable
            #   '';
            # }
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
              option = ".*\.sbt";
              commands = "set buffer filetype scala";
            }
            {
              name = "BufCreate";
              option = "go\.(mod|sum)";
              commands = "set buffer filetype go";
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
            # {
            #   name = "WinSetOption";
            #   option = "filetype=scala";
            #   commands = "hook window BufWritePre .* lsp-formatting-sync";
            # }
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
                    map window insert <tab> <c-n>
                    map window insert <s-tab> <c-p>
                    hook -once -always window InsertCompletionHide .* %{
                      unmap window insert <tab> <c-n>
                      unmap window insert <s-tab> <c-p>
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
            # {
            #   mode = "window";
            #   key = "h";
            #   effect = ":nop %sh{kitten @ focus-window --no-response --match=neighbor:left}<ret>";
            #   docstring = "select pane left";
            # }
            # {
            #   mode = "window";
            #   key = "j";
            #   effect = ":nop %sh{kitten @ focus-window --no-response --match=neighbor:bottom}<ret>";
            #   docstring = "select pane down";
            # }
            # {
            #   mode = "window";
            #   key = "k";
            #   effect = ":nop %sh{kitten @ focus-window --no-response --match=neighbor:top}<ret>";
            #   docstring = "select pane up";
            # }
            # {
            #   mode = "window";
            #   key = "l";
            #   effect = ":nop %sh{kitten @ focus-window --no-response --match=neighbor:right}<ret>";
            #   docstring = "select pane right";
            # }
            # {
            #   mode = "window";
            #   key = "p";
            #   effect = ":nop %sh{kitten @ focus-window --no-response --match=recent:1}<ret>";
            #   docstring = "select previous pane";
            # }
            {
              mode = "window";
              key = "s";
              effect = ":new<ret>";
              docstring = "split window";
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
            {
              mode = "user";
              key = "f";
              effect = ":find<ret>";
              docstring = "find file";
            }
          ];
      };
      defaultEditor = true;
      # extraConfig = ''
      #   # eval %sh{kak-lsp --kakoune -s $kak_session}

      #   define-command find -docstring "find file" -params .. %{
      #     kitty-overlay --copy-env sk --bind %exp{enter:execute(echo eval -verbatim -client %val{client} edit '"{}"' | kak -p %val{session})+abort}
      #   }

      #   define-command kitty-overlay -params 1.. -docstring '
      #   kitty-overlay <program> [<arguments>]: create a new terminal as a kitty overlay
      #   The program passed as argument will be executed in the new terminal' \
      #   %{
      #       nop %sh{
      #           match=""
      #           if [ -n "$kak_client_env_KITTY_WINDOW_ID" ]; then
      #               match="--match=window_id:$kak_client_env_KITTY_WINDOW_ID"
      #           fi

      #           listen=""
      #           if [ -n "$kak_client_env_KITTY_LISTEN_ON" ]; then
      #               listen="--to=$kak_client_env_KITTY_LISTEN_ON"
      #           fi

      #           kitty @ $listen launch --no-response --type="overlay" --cwd="$PWD" $match "$@"
      #       }
      #   }
      #   complete-command kitty-overlay shell
      # '';
      plugins = [
        pkgs.kakounePlugins.kakoune-lsp
      #   pkgs.kakounePlugins.kakboard
      ];
    };

    # kitty = {
    #   enable = true;
    #   darwinLaunchOptions = [
    #     "--single-instance"
    #   ];
    #   extraConfig = ''
    #     include current-theme.conf
    #   '';
    #   font = {
    #     # name = "Berkeley Mono";
    #     name = "Monaspace Neon Regular";
    #     size = 14;
    #   };
    #   keybindings = {
    #     "cmd+ctrl+shift+[" = "move_tab_backward";
    #     "cmd+ctrl+shift+]" = "move_tab_forward";
    #     "cmd+ctrl+enter" = "next_layout";
    #   };
    #   settings = {
    #     allow_remote_control = true;
    #     inactive_text_alpha = "0.85";
    #     update_check_interval = 0;
    #     macos_option_as_alt = true;
    #     tab_bar_style = "separator";
    #     tab_bar_min_tabs = 1;
    #     tab_separator = "''";
    #     active_tab_font_style = "normal";
    #     inactive_tab_font_style = "normal";
    #     tab_title_max_length = 0;
    #     tab_title_template = "' {fmt.fg.red}{bell_symbol}{activity_symbol}{fmt.fg.tab}{index}: {title:^15.15} '";
    #     # shell = "/Users/diegs/.nix-profile/bin/fish --login --interactive";
    #   };
    # };

    readline = {
      enable = true;
      variables = {
        show-all-if-ambiguous = true;
        page-completions = false;
      };
    };

    skim = {
      enable = true;
      changeDirWidgetCommand = "fd -H --type d --color=always";
      changeDirWidgetOptions = ["--ansi" "--height 100%" "--preview 'tree -C {} | head -200'"];
      defaultCommand = "fd -H --type f --color=always";
      defaultOptions = ["--ansi" "--height 100%" "--preview 'bat -f --style=numbers {}'"];
      fileWidgetCommand = "fd -H --type f --color=always";
      fileWidgetOptions = ["--ansi" "--height 100%" "--preview 'bat -f --style=numbers {}'"];
      historyWidgetOptions = [];
    };

    ssh = {
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

    zellij = {
      enable = true;
    };

    zoxide = {
      enable = true;
    };
  };

  xdg = {
    enable = true;
  };

}
