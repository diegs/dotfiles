{ pkgs, ... }:
{
  home = {
    stateVersion = "23.11";

    sessionPath = [
      "$HOME/.go/bin"
      "$HOME/.local/bin"
    ];

    packages = [
      # util
      pkgs.fd
      pkgs.jq
      pkgs.hexyl
      pkgs.pure-prompt
      pkgs.ripgrep
      pkgs.sd
      pkgs.tree
      pkgs.watch
      pkgs.wget
      pkgs.yq-go

      # lsp
      (pkgs.aspellWithDicts (d: [ d.en ]))
      pkgs.bash-language-server
      pkgs.cmake-language-server
      pkgs.dockerfile-language-server-nodejs
      pkgs.gopls
      pkgs.nil
      pkgs.nodePackages.vscode-json-languageserver
      pkgs.yaml-language-server

      # lint
      pkgs.golangci-lint

      # nix
      pkgs.cachix

      (pkgs.writeShellScriptBin "e" ''
        emacsclient -nw "$@"
      '')
      (pkgs.writeShellScriptBin "ec" ''
        emacsclient -n -r "$@"
      '')
      # (pkgs.writeShellScriptBin "ec" ''
      #   emacsclient -n -e "(> (length (frame-list)) 1)" | grep -q t
      #   if [ "$?" = "1" ]; then
      #     emacsclient -c -n "$@"
      #   else
      #     emacsclient -n "$@"
      #     # emacsclient -n -e  "(select-frame-set-input-focus (selected-frame))" > /dev/null
      #   fi
      # '')
    ];

    file = {
      ".config/1Password/ssh/agent.toml".text = ''
        [[ssh-keys]]
        vault = "Private"
      '';
      ".hushlogin".text = "";
      ".ignore".text = ".git/";
    };

    sessionVariables = {
      # XDG_DATA_DIRS = "$HOME/.nix-profile/share:$XDG_DATA_DIRS";
      # FLAKE_CONFIG_URI = "path:${config.home.homeDirectory}/src/dotfiles";
      # SSH_AUTH_SOCK = "$HOME/Library/Group\ Containers/2BUA8C4S2C.com.1password/t/agent.sock";
    };
  };

  fonts.fontconfig.enable = false;

  programs = {
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
        decorations = "never";
        pager = "";
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

    emacs = {
      enable = false;
      package = (
        pkgs.emacsWithPackagesFromUsePackage {
          config = ./emacs.el;
          defaultInitFile = true;
          # package = if pkgs.stdenv.isDarwin then pkgs.emacs30 else pkgs.emacs30-pgtk;
          package = (pkgs.emacs30-pgtk.override { withNativeCompilation = false; });
          alwaysEnsure = true;
          extraEmacsPackages = epkgs: [
            epkgs.treesit-grammars.with-all-grammars
          ];
        }
      );
    };

    eza = {
      enable = true;
    };

    ghostty = {
      enable = true;
      package = if pkgs.stdenv.isDarwin then null else pkgs.ghostty;
      settings = {
        #theme = "dark:GitHub-Dark-Dimmed,light:GitHub-Light-Default";
        theme = "light:modus-operandi,dark:modus-vivendi";
        font-size = 12;
        auto-update = "off";
      };
      themes = {
        modus-operandi = {
          background = "#ffffff";
          foreground = "#000000";
          selection-background = "#bdbdbd";
          selection-foreground = "#000000";
          cursor-color = "#000000";
          palette = [
            # black
            "0=#000000"
            "8=#595959"
            # red
            "1=#a60000"
            "9=#a0132f"
            # green
            "2=#006800"
            "10=#00663f"
            # yellow
            "3=#6f5500"
            "11=#7a4f2f"
            # blue
            "4=#0031a9"
            "12=#0000b0"
            # magenta
            "5=#721045"
            "13=#531ab6"
            # cyan
            "6=#005e8b"
            "14=#005f5f"
            # white
            "7=#f2f2f2"
            "15=#c4c4c4"
          ];
        };
        modus-vivendi = {
          background = "#111111";
          foreground = "#ffffff";
          selection-background = "#5a5a5a";
          selection-foreground = "#ffffff";
          cursor-color = "#ffffff";
          palette = [
            # black
            "0=#1e1e1e"
            "8=#535353"
            # red
            "1=#ff5f59"
            "9=#ff7f9f"
            # green
            "2=#44bc44"
            "10=#00c06f"
            # yellow
            "3=#d0bc00"
            "11=#dfaf7a"
            # blue
            "4=#2fafff"
            "12=#00bcff"
            # magenta
            "5=#feacd0"
            "13=#b6a0ff"
            # cyan
            "6=#00d3d0"
            "14=#6ae4b9"
            # white
            "7=#ffffff"
            "15=#989898"
          ];
        };
      };
    };

    git = {
      enable = true;
      userName = "Diego Pontoriero";
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
            program =
              if pkgs.stdenv.isDarwin then
                "/Applications/1Password.app/Contents/MacOS/op-ssh-sign"
              else
                "/opt/1Password/op-ssh-sign";
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
      aliases = {
        mr = "!sh -c 'git fetch $1 merge-requests/$2/head:mr-$1-$2 && git checkout mr-$1-$2' -";
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

    jujutsu = {
      enable = false;
      settings = {
        ui = {
          default-command = "log";
        };
        user = {
          name = "Diego Pontoriero";
        };
      };
    };

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
      changeDirWidgetOptions = [
        "--ansi"
        "--height 100%"
        "--preview 'tree -C {} | head -200'"
      ];
      defaultCommand = "fd -H --type f --color=always";
      defaultOptions = [
        "--ansi"
        "--height 100%"
        "--preview 'bat --decorations=always --color=always --style=numbers {}'"
      ];
      fileWidgetCommand = "fd -H --type f --color=always";
      fileWidgetOptions = [
        "--ansi"
        "--height 100%"
        "--preview 'bat --decorations=always --color=always --style=numbers {}'"
      ];
      historyWidgetOptions = [ ];
    };

    ssh =
      let
        identityAgent =
          if pkgs.stdenv.isDarwin then
            "~/Library/Group\\ Containers/2BUA8C4S2C.com.1password/t/agent.sock"
          else
            "~/.1password/agent.sock";
      in
      {
        enable = true;
        compression = true;
        controlMaster = "auto";
        controlPath = "~/.ssh/tmp/%r@%n:%p";
        controlPersist = "4h";
        forwardAgent = true;
        extraOptionOverrides = {
          Include = "config.d/*";
          AddKeysToAgent = "yes";
          StrictHostKeyChecking = "no";
          IdentityAgent = identityAgent;
        } // (if pkgs.stdenv.isDarwin then { UseKeychain = "yes"; } else { });
        matchBlocks = {
          "192.168.*" = {
            extraOptions = {
              HostKeyAlgorithms = "+ssh-rsa";
              IdentityAgent = identityAgent;
              PubkeyAcceptedKeyTypes = "+ssh-rsa";
              KexAlgorithms = "+diffie-hellman-group1-sha1";
            };
          };
          "172.17.*" = {
            extraOptions = {
              HostKeyAlgorithms = "+ssh-rsa";
              IdentityAgent = identityAgent;
              PubkeyAcceptedKeyTypes = "+ssh-rsa";
              KexAlgorithms = "+diffie-hellman-group1-sha1";
            };
          };
          "*.home.arpa" = {
            user = "admin";
            extraOptions = {
              HostKeyAlgorithms = "+ssh-rsa";
              IdentityAgent = identityAgent;
              PubkeyAcceptedKeyTypes = "+ssh-rsa";
              KexAlgorithms = "+diffie-hellman-group1-sha1";
            };
          };
          "github.com" = {
            hostname = "ssh.github.com";
            port = 443;
            extraOptions = {
              IdentityAgent = identityAgent;
            };
          };
          "gitlab.com" = {
            extraOptions = {
              IdentityAgent = identityAgent;
            };
          };
          "gitlab-master.nvidia.com" = {
            extraOptions = {
              IdentityAgent = identityAgent;
            };
          };
        };
      };

    uv = {
      enable = true;
    };

    zoxide = {
      enable = true;
    };

    zsh = {
      enable = true;
      defaultKeymap = "emacs";
      envExtra = ''
        if test -d /opt/homebrew; then
          eval "$(/opt/homebrew/bin/brew shellenv)"
        fi
      '';
      initContent = ''
        autoload -U promptinit; promptinit
        zstyle :prompt:pure:git:stash show yes
        prompt pure

        autoload -U edit-command-line
        zle -N edit-command-line
        bindkey "^X^E" edit-command-line
      '';
      sessionVariables = {
        EDITOR = "emacs -nw";
        VISUAL = "emacs -nw";
      };
      shellAliases = {
        cat = "bat";
      };
    };
  };

  xdg = {
    enable = true;
  };
}
