{ config, lib, pkgs, ... }:
{
  home = {
    stateVersion = "23.11";

    sessionPath = [ "$HOME/.go/bin" ]; # "/opt/homebrew/bin" ];

    packages = [
      # util
      pkgs.fd
      pkgs.hexyl
      pkgs.pure-prompt
      pkgs.ripgrep
      pkgs.sd
      pkgs.tree
      pkgs.watch
      pkgs.wget

      # ui
      # pkgs.monaspace

      # dev tools
      pkgs.cachix
      pkgs.nil
      pkgs.jq
      pkgs.yq
      # pkgs.yq-go
      # pkgs.colima
      # pkgs.docker-client
      # pkgs.docker-buildx

      # markdown
      pkgs.marksman

      # k8s
      pkgs.awscli2
      (pkgs.google-cloud-sdk.withExtraComponents(with pkgs.google-cloud-sdk.components; [
        beta
        gke-gcloud-auth-plugin
      ]))
      pkgs.packer
      pkgs.kubectl
      pkgs.kustomize
      pkgs.kubernetes-helm
      # pkgs.setup-envtest
      # pkgs.kubernetes-controller-tools

      pkgs.go-task
      pkgs.yamllint
      pkgs.terraform
      pkgs.tflint
      pkgs.terraform-docs
      pkgs.glab
      pkgs.kind

      # go
      pkgs.golangci-lint
      pkgs.gopls
      pkgs.gocover-cobertura
      # pkgs.gotools

      (pkgs.writeShellScriptBin "e" ''
        emacsclient -n -e "(> (length (frame-list)) 1)" | grep -q t
        if [ "$?" = "1" ]; then
          emacsclient -c -n -a "" "$@"
        else
          emacsclient -n -a "" "$@"
        fi
      '')
      # protobuf
      # pkgs.protobuf_26

      # python
      # pkgs.pyright
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
      enable = true;
      package = (pkgs.emacsWithPackagesFromUsePackage {
        config = ./emacs.el;
        defaultInitFile = true;
        package = pkgs.emacs-30;
        alwaysEnsure = true;
        extraEmacsPackages = epkgs: [
          epkgs.treesit-grammars.with-all-grammars
        ];
      });
    };

    eza = {
      enable = true;
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

    home-manager = {
      enable = false;
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
      changeDirWidgetOptions = ["--ansi" "--height 100%" "--preview 'tree -C {} | head -200'"];
      defaultCommand = "fd -H --type f --color=always";
      defaultOptions = ["--ansi" "--height 100%" "--preview 'bat --decorations=always --color=always --style=numbers {}'"];
      fileWidgetCommand = "fd -H --type f --color=always";
      fileWidgetOptions = ["--ansi" "--height 100%" "--preview 'bat --decorations=always --color=always --style=numbers {}'"];
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
        "github.com" = {
          hostname = "ssh.github.com";
          port = 443;
        };
      };
    };

    zoxide = {
      enable = true;
    };

    zsh = {
      enable = true;
      initExtra = ''
        autoload -U promptinit; promptinit
        zstyle :prompt:pure:git:stash show yes
        prompt pure

        autoload -U edit-command-line
        zle -N edit-command-line
        bindkey "^X^E" edit-command-line

        if test -d /opt/homebrew; then
          eval "$(/opt/homebrew/bin/brew shellenv)"
        fi
      '';
      sessionVariables = {
        EDITOR = "vim";
      };
      shellAliases = {
        cat = "bat";
        k = "kubectl";
      };
    };
  };

  xdg = {
    enable = true;
  };
}
