{ config, pkgs, lib, ... }:

let
  customPlugins.salt-vim = pkgs.vimUtils.buildVimPlugin {
    name = "salt-vim";
    src = pkgs.fetchFromGitHub {
      owner = "saltstack";
      repo = "salt-vim";
      rev = "6ca9e3500cc39dd417b411435d58a1b720b331cc";
      sha256 = "0r79bpl98xcsmkw6dg83cf1ghn89rzsr011zirk3v1wfxclri2c4";
    };
  };
  customPlugins.tig-explorer-vim = pkgs.vimUtils.buildVimPlugin {
    name = "tig-explorer-vim";
    src = pkgs.fetchFromGitHub {
      owner = "iberianpig";
      repo = "tig-explorer.vim";
      rev = "52052311b317117d0f0b8c1cc8d1760767f61277";
      sha256 = "1j346l8jh1bya5nmw9jrv2vsb5piz3wgw0jmkmd41iq8ay4zjfnh";
    };
  };
  customPlugins.vim-asterisk = pkgs.vimUtils.buildVimPlugin {
    name = "vim-asterisk";
    src = pkgs.fetchFromGitHub {
      owner = "haya14busa";
      repo = "vim-asterisk";
      rev = "77e97061d6691637a034258cc415d98670698459";
      sha256 = "1bm99j4vskbgzfn09567qi0462dvjrpdkifc4hg24bi02bx9hjrj";
    };
  };
  customPlugins.vim-bufkill = pkgs.vimUtils.buildVimPlugin {
    name = "vim-bufkill";
    src = pkgs.fetchFromGitHub {
      owner = "qpkorr";
      repo = "vim-bufkill";
      rev = "2bd6d7e791668ea52bb26be2639406fcf617271f";
      sha256 = "1cvma03bg9psil67kg1x90lny7a31ljz5shybcl1jrfpzsybcqvg";
    };
  };
  customPlugins.vim-textobj-user = pkgs.vimUtils.buildVimPlugin {
    name = "vim-textobj-user";
    src = pkgs.fetchFromGitHub {
      owner = "kana";
      repo = "vim-textobj-user";
      rev = "41a675ddbeefd6a93664a4dc52f302fe3086a933";
      sha256 = "1y1g3vcm97fqjyigiajbvbck4nlc04vxl3535x4sl40s5jbm5vz3";
    };
  };
  customPlugins.vim-textobj-comment = pkgs.vimUtils.buildVimPlugin {
    name = "vim-textobj-comment";
    configurePhase = ''
      rm Makefile
    '';
    src = pkgs.fetchFromGitHub {
      owner = "glts";
      repo = "vim-textobj-comment";
      rev = "58ae4571b76a5bf74850698f23d235eef991dd4b";
      sha256 = "00wc14chwjfx95gl3yzbxm1ajx88zpzqz0ckl7xvd7gvkrf0mx04";
    };
  };
  extraPkgs.gopls = pkgs.buildGoModule rec {
    pname = "gopls";
    version = "0.5.1";

    src = pkgs.fetchgit {
      rev = "gopls/v${version}";
      url = "https://go.googlesource.com/tools";
      sha256 = "150jg1qmdszfvh1x5fagawgc24xy19xjg9y1hq3drwy7lfdnahmq";
    };

    modRoot = "gopls";
    vendorSha256 = "1s3d4hnbw0mab7njck79qmgkjn87vs4ffk44zk2qdrzqjjlqq5iv";

    doCheck = false;

    meta = with pkgs.stdenv.lib; {
      description = "Official language server for the Go language";
      homepage = "https://github.com/golang/tools/tree/master/gopls";
      license = licenses.bsd3;
      maintainers = with maintainers; [ mic92 zimbatm ];
    };
  };
in {
  targets.genericLinux = {
    enable = true;
    # extraXdgDataDirs = [
    #   "/usr/share/pop"
    #   "/home/diegs/.local/share/flatpak/exports/share"
    #   "/var/lib/flatpak/exports/share"
    #   "/usr/local/share"
    #   "/usr/share"
    # ];
  };

  fonts.fontconfig.enable = true;

  home = {
    extraOutputsToInstall = [ "man" ];
    language = {
      base = "en_US.UTF-8";
    };
    packages = [
      # util
      pkgs.awscli
      pkgs.bash-completion
      pkgs.ctags
      pkgs.fd
      pkgs.exa
      pkgs.cascadia-code
      pkgs.glibcLocales
      pkgs.hexyl
      pkgs.inotify-tools
      pkgs.graphviz
      pkgs.neofetch
      pkgs.ripgrep
      pkgs.xclip
      pkgs.tig
      pkgs.tree

      # go
      pkgs.golangci-lint
      extraPkgs.gopls
      pkgs.gotags
      pkgs.gotools

      # haskell
      pkgs.cabal-install
      pkgs.cabal2nix
      pkgs.nix-prefetch-git
    ];
    sessionVariables = {
      EDITOR = "vim";
      LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
      VISUAL = "vim";
      # XDG_DATA_DIRS = "$HOME/.nix-profile/share:$XDG_DATA_DIRS";
    };
  };

  home.file.".config/alacritty/alacritty.yml" = {
    source = ../../.config/alacritty/alacritty.yml;
  };

  programs.bash = {
    enable = true;
    enableVteIntegration = true;
    historyControl = ["ignoredups" "erasedups"];
    initExtra = lib.mkBefore ''
      # Base16 Shell
      BASE16_SHELL="$HOME/.config/base16-shell/"
      [ -n "$PS1" ] && \
      [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
        eval "$("$BASE16_SHELL/profile_helper.sh")"

      # Completions
      source $HOME/.nix-profile/etc/profile.d/bash_completion.sh
    '';
    profileExtra = ''
      if [ -f ~/.bash_local ]; then
        . ~/.bash_local
      fi
    '';
    shellAliases = {
      ls = "exa";
      ll = "ls -alF";
      la = "ls -aa";
      l = "ls -F";
      tm = "tmux a";
      cat = "bat";
      colors = ''for i in {0..255}; do printf "\x1b[38;5;$''\{i}mcolor%-5i\x1b[0m" $i ; if ! (( ($i + 1 ) % 8 )); then echo ; fi ; done'';
    };
  };

  programs.bat = {
    enable = true;
  };

  programs.command-not-found = {
    enable = true;
  };

  programs.direnv = {
    enable = true;
    enableNixDirenvIntegration = true;
    stdlib = ''
      layout_virtualenv() {
        local venv_path="venv"
        source $''\{venv_path}/bin/activate
      }
    '';
  };

  programs.emacs = {
    enable = false;
    package = pkgs.emacs-nox;
    extraPackages = epkgs: [
      # epkgs.better-defaults
      epkgs.base16-theme
      epkgs.company
      epkgs.counsel
      epkgs.evil
      epkgs.flycheck
      epkgs.go-mode
      epkgs.goto-chg
      epkgs.ivy
      epkgs.ivy-hydra
      epkgs.lsp-mode
      epkgs.magit
      epkgs.nix-mode
      epkgs.projectile
      epkgs.undo-tree
      epkgs.yaml-mode
    ];
  };
  home.file.".emacs.d" = {
    source = ../../.emacs.d;
  };

  programs.feh = {
    enable = true;
  };

  programs.fzf = {
    enable = true;
    changeDirWidgetCommand = "fd --type d";
    defaultCommand = "fd --type f";
    fileWidgetCommand = "fd --type f";
  };

  programs.git = {
    enable = true;
    userName = "Diego Pontoriero";
    userEmail = "74719+diegs@users.noreply.github.com";
    aliases = {
      br = "branch";
      ci = "commit";
      co = "checkout";
      cp = "cherry-pick";
      st = "status";
    };
    attributes = [
      # https://gist.github.com/tekin/12500956bd56784728e490d8cef9cb81
      "*.c     diff=cpp"
      "*.h     diff=cpp"
      "*.c++   diff=cpp"
      "*.h++   diff=cpp"
      "*.cpp   diff=cpp"
      "*.hpp   diff=cpp"
      "*.cc    diff=cpp"
      "*.hh    diff=cpp"
      "*.m     diff=objc"
      "*.mm    diff=objc"
      "*.cs    diff=csharp"
      "*.css   diff=css"
      "*.html  diff=html"
      "*.xhtml diff=html"
      "*.ex    diff=elixir"
      "*.exs   diff=elixir"
      "*.go    diff=golang"
      "*.php   diff=php"
      "*.pl    diff=perl"
      "*.py    diff=python"
      "*.md    diff=markdown"
      "*.rb    diff=ruby"
      "*.rake  diff=ruby"
      "*.rs    diff=rust"
      "*.lisp  diff=lisp"
      "*.el    diff=lisp"
    ];
    extraConfig = {
      fetch = { prune = true; tags = true; };
      init = { templateDir = "~/.git-template"; };
      pull = { rebase = true; };
      push = { default = "current"; };
      url."git@github.com:".insteadOf = "https://github.com/";
    };
    ignores = [
      ".envrc"
      ".mypy_cache/"
    ];
  };

  programs.go = {
    enable = true;
    goPath = ".";
  };

  programs.home-manager = {
    enable = true;
  };

  programs.htop = {
    enable = true;
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

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    extraConfig = lib.strings.fileContents ../nvim/init.vim;
    plugins = [
      customPlugins.salt-vim
      customPlugins.tig-explorer-vim
      customPlugins.vim-asterisk
      customPlugins.vim-bufkill
      customPlugins.vim-textobj-comment
      customPlugins.vim-textobj-user
      pkgs.vimPlugins.ale
      pkgs.vimPlugins.base16-vim
      pkgs.vimPlugins.bclose-vim
      pkgs.vimPlugins.fzf-vim
      pkgs.vimPlugins.fzfWrapper
      # pkgs.vimPlugins.goyo-vim
      pkgs.vimPlugins.haskell-vim
      pkgs.vimPlugins.tagbar
      pkgs.vimPlugins.vim-abolish
      pkgs.vimPlugins.vim-airline
      pkgs.vimPlugins.vim-airline-themes
      pkgs.vimPlugins.vim-commentary
      pkgs.vimPlugins.vim-easy-align
      # pkgs.vimPlugins.vim-fugitive
      # pkgs.vimPlugins.vim-orgmode
      pkgs.vimPlugins.vim-polyglot
      pkgs.vimPlugins.vim-repeat
      pkgs.vimPlugins.vim-sensible
      pkgs.vimPlugins.vim-surround
      pkgs.vimPlugins.vim-tmux-navigator
      pkgs.vimPlugins.vim-unimpaired
      pkgs.vimPlugins.vim-vinegar
    ];
  };

  programs.readline = {
    enable = true;
    variables = {
      editing-mode = "vi";
    };
  };

  programs.starship = {
    enable = true;
    settings = {
      aws = {
        symbol = "";
      };
      battery = {
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
      env_var = {
        format = "[$env_value]($style) ";
        style = "yellow";
        variable = "AWS_OKTA_PROFILE";
      };
      git_branch = {
        format = "[$symbol$branch]($style) ";
        symbol = "";
      };
      golang = {
        disabled = true;
      };
      python = {
        disabled = true;
      };
    };
  };

  programs.tmux = {
    enable = true;
    baseIndex = 1;
    clock24 = true;
    escapeTime = 20;
    keyMode = "vi";
    newSession = true;
    terminal = "screen-256color";
    plugins = with pkgs; [
      tmuxPlugins.copycat
      {
        plugin = tmuxPlugins.resurrect;
        extraConfig = "set -g @resurrect-strategy-nvim 'session'";
      }
      {
        plugin = tmuxPlugins.continuum;
        extraConfig = ''
          set -g @continuum-restore 'on'
          set -g @continuum-save-interval '60' # minutes
        '';
      }
      tmuxPlugins.vim-tmux-navigator
      tmuxPlugins.yank
    ];
    extraConfig = lib.strings.fileContents ../../.tmux.conf;
  };
  # tmux clipboardy things to maybe try to get working someday.
  # 'tmux-plugins/vim-tmux-focus-events'
  # 'roxma/vim-tmux-clipboard'

  xdg.userDirs = {
    enable = true;
    desktop = "\"$HOME/.desktop\"";
    documents = "\"$HOME\"";
    download = "\"$HOME/downloads\"";
    music = "\"$HOME/music\"";
    pictures = "\"$HOME\"";
    publicShare = "\"$HOME\"";
    templates = "\"$HOME\"";
    videos = "\"$HOME\"";
  };
}
