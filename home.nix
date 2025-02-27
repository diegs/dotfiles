{ pkgs, ghostty, ... }:
let
  oci-cli = pkgs.oci-cli.overridePythonAttrs (old: rec {
    inherit (old) pname;
    version = "3.50.1";
    src = pkgs.fetchFromGitHub {
      owner = "oracle";
      repo = pname;
      rev = "v${version}";
      hash = "sha256-qSXIHEdNIfEPDsXIeHqfl49yjnQ320EiVNRapBQX4vQ=";
    };
  });
in
{
  home = {
    stateVersion = "23.11";

    sessionPath = [ "$HOME/.go/bin" "$HOME/.local/bin"];

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
      pkgs.pre-commit

      # ui
      # pkgs.monaspace

      # dev tools
      pkgs.bash-language-server
      pkgs.cmake-language-server
      pkgs.dockerfile-language-server-nodejs
      pkgs.nodePackages.vscode-json-languageserver
      pkgs.yaml-language-server
      pkgs.cachix
      pkgs.git-branchless
      pkgs.nil
      pkgs.jq
      # pkgs.yq
      pkgs.yq-go
      # pkgs.colima
      # pkgs.docker-client
      # pkgs.docker-buildx
      pkgs.gomplate

      # markdown
      pkgs.marksman
      pkgs.pandoc

      # k8s
      pkgs.argocd
      pkgs.awscli2
      pkgs.cilium-cli
      oci-cli
      (pkgs.google-cloud-sdk.withExtraComponents(with pkgs.google-cloud-sdk.components; [
        beta
        gke-gcloud-auth-plugin
      ]))
      pkgs.packer
      pkgs.kubectl
      pkgs.kubelogin
      pkgs.kustomize
      pkgs.kubernetes-helm
      # pkgs.setup-envtest
      # pkgs.kubernetes-controller-tools

      pkgs.go-task
      pkgs.yamllint
      pkgs.terraform
      pkgs.terraform-ls
      pkgs.tflint
      pkgs.terraform-docs
      pkgs.trivy
      pkgs.glab
      pkgs.kind

      # go
      pkgs.golangci-lint
      pkgs.gopls
      pkgs.gocover-cobertura
      # pkgs.gotools

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
      enable = true;
      package = (pkgs.emacsWithPackagesFromUsePackage {
        config = ./emacs.el;
        defaultInitFile = true;
        package = if pkgs.stdenv.isDarwin then pkgs.emacs30 else pkgs.emacs30-pgtk;
        alwaysEnsure = true;
        extraEmacsPackages = epkgs: [
          epkgs.treesit-grammars.with-all-grammars
        ];
      });
    };

    eza = {
      enable = true;
    };

    ghostty = {
      enable = true;
      package = null; # ghostty.packages.x86_64-linux.default;
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
            program = if pkgs.stdenv.isDarwin then "/Applications/1Password.app/Contents/MacOS/op-ssh-sign" else "/opt/1Password/op-ssh-sign";
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
      changeDirWidgetOptions = ["--ansi" "--height 100%" "--preview 'tree -C {} | head -200'"];
      defaultCommand = "fd -H --type f --color=always";
      defaultOptions = ["--ansi" "--height 100%" "--preview 'bat --decorations=always --color=always --style=numbers {}'"];
      fileWidgetCommand = "fd -H --type f --color=always";
      fileWidgetOptions = ["--ansi" "--height 100%" "--preview 'bat --decorations=always --color=always --style=numbers {}'"];
      historyWidgetOptions = [];
    };

    ssh = let
      identityAgent = if pkgs.stdenv.isDarwin then "~/Library/Group\\ Containers/2BUA8C4S2C.com.1password/t/agent.sock" else "~/.1password/agent.sock";
    in {
      enable = true;
      forwardAgent = true;
      extraOptionOverrides = {
        AddKeysToAgent = "yes";
        StrictHostKeyChecking = "no";
      } // (if pkgs.stdenv.isDarwin then { UseKeychain = "yes"; } else {});
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

    zoxide = {
      enable = true;
    };

    zsh = {
      enable = true;
      defaultKeymap = "emacs";
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
        EDITOR = "e";
        VISUAL = "e";
      };
      shellAliases = {
        cat = "bat";
        k = "kubectl";
        tf = "terraform";
      };
    };
  };

  xdg = {
    enable = true;
  };
}
