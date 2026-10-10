{ pkgs, config, ... }:
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
      pkgs.dasel
      pkgs.dust
      pkgs.hexyl
      pkgs.pure-prompt
      pkgs.ripgrep
      pkgs.sd
      pkgs.tree
      pkgs.watch
      pkgs.wget
      pkgs.yq-go

      # lsp / tools
      (pkgs.aspellWithDicts (d: [ d.en ]))
      pkgs.bash-language-server
      pkgs.nil

      # nix
      pkgs.cachix
      pkgs.comma
      pkgs.nixfmt
    ];

    file = {
      ".config/1Password/ssh/agent.toml".text = ''
        [[ssh-keys]]
        vault = "Private"
      '';
      ".hushlogin".text = "";
      ".ignore".text = ".git/";
    };
  };

  fonts.fontconfig.enable = false;
  manual.manpages.enable = false;

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

    btop = {
      enable = true;
    };

    delta = {
      enable = true;
      enableGitIntegration = true;
      options = {
        navigate = true;
        syntax-theme = "ansi";
        minus-style = "reverse red";
        minus-emph-style = "reverse bold red";
        plus-style = "reverse green";
        plus-emph-style = "reverse bold green";
      };
    };

    dircolors = {
      enable = false;
    };

    direnv = {
      enable = true;
      nix-direnv = {
        enable = true;
      };
    };

    ghostty = {
      enable = true;
      package = if pkgs.stdenv.hostPlatform.isDarwin then null else pkgs.ghostty;
      settings = {
        theme = "light:Apple System Colors Light,dark:Apple System Colors";
        auto-update = "off";
      };
    };

    git = {
      enable = true;
      settings = {
        advice = {
          addIgnoredFile = false;
        };
        alias = {
          mr = "!sh -c 'git fetch $1 merge-requests/$2/head:mr-$1-$2 && git checkout mr-$1-$2' -";
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
              if pkgs.stdenv.hostPlatform.isDarwin then
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
        user = {
          name = "Diego Pontoriero";
          email = "74719+diegs@users.noreply.github.com";
          signingkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILJasnFrDOljlqzQUCWT34ci8fp5/QgYh2QWvJM2l942";
        };
        url = {
          "ssh://git@github.com/" = {
            insteadOf = "https://github.com/";
          };
        };
      };
      ignores = [
        ".direnv/"
        ".DS_Store"
      ];
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
        signing = {
          behavior = "own";
          backend = "ssh";
          key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILJasnFrDOljlqzQUCWT34ci8fp5/QgYh2QWvJM2l942";
          backends.ssh.program =
            if pkgs.stdenv.hostPlatform.isDarwin then
              "/Applications/1Password.app/Contents/MacOS/op-ssh-sign"
            else
              "/opt/1Password/op-ssh-sign";
        };
      };
    };

    kakoune = {
      enable = true;
      defaultEditor = true;
      config = {
          indentWidth = 2;
          tabStop = 2;
          numberLines = {
              enable = true;
              relative = true;
          };
      };
    };

    lsd = {
      enable = true;
      colors = {
        user = "cyan";
        group = "dark_cyan";
        permission = {
          no-access = "dark_magenta";
        };
        date = {
          hour-old = "dark_cyan";
          day-old = "dark_blue";
          older = "dark_green";
        };
        size = {
          none = "grey";
          small = "dark_yellow";
          medium = "dark_magenta";
          large = "dark_red";
        };
        inode = {
          invalid = "grey";
        };
        links = {
          invalid = "grey";
        };
        tree-edge = "grey";
        git-status = {
          default = "grey";
          unmodified = "grey";
          ignored = "grey";
        };
      };
      settings = {
        icons = {
          when = "never";
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

    fzf = {
      enable = true;
      enableZshIntegration = true;
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

    nh = {
      enable = true;
      flake = "${config.home.homeDirectory}/dev/dotfiles";
      clean = {
        enable = true;
        dates = "weekly";
        extraArgs = "--keep-one";
      };
    };

    nix-index = {
      enable = true;
      enableZshIntegration = true;
    };

    ssh =
      let
        identityAgent =
          if pkgs.stdenv.hostPlatform.isDarwin then
            "~/Library/Group\\ Containers/2BUA8C4S2C.com.1password/t/agent.sock"
          else
            "~/.1password/agent.sock";
      in
      {
        enable = true;
        enableDefaultConfig = false;
        includes = [ "conf.d/*" ];
        settings = {
          "*.home.diegs.ca" = {
            User = "admin";
            HostKeyAlgorithms = "+ssh-rsa";
            PubkeyAcceptedKeyTypes = "+ssh-rsa";
            KexAlgorithms = "+diffie-hellman-group1-sha1";
          };
          "github.com" = {
            HostName = "ssh.github.com";
            Port = 443;
          };
          "*" = {
            Compression = true;
            ControlMaster = "auto";
            ControlPath = "~/.ssh/ctl-%r@%n:%p";
            ControlPersist = "4h";
            HashKnownHosts = false;
            UserKnownHostsFile = "~/.ssh/known_hosts";
            ForwardAgent = true;
            AddKeysToAgent = "yes";
            IdentityAgent = identityAgent;
          } // (if pkgs.stdenv.hostPlatform.isDarwin then { UseKeychain = "yes"; } else { });
        };
      };

    zsh = {
      dotDir = "${config.xdg.configHome}/zsh";
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
      shellAliases = {
        cat = "bat";
      };
      plugins = [
        {
          name = "fzf-tab";
          src = pkgs.zsh-fzf-tab;
          file = "share/fzf-tab/fzf-tab.plugin.zsh";
        }
      ];
    };

    zoxide = {
      enable = true;
      enableZshIntegration = true;
    };
  };

  xdg = {
    enable = true;
  };
}
