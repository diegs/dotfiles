{ config, pkgs, ... }:

let
  unstable = import <unstable> {};
in {
  imports = [ ./hardware-configuration.nix ];

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot.enable = true;
    };
  };

  environment.systemPackages = with pkgs; [
    # Dev.
    unstable.awscli
    unstable.bazel
    binutils
    unstable.cargo
    unstable.dep
    git
    unstable.glide
    glibc
    gnumake
    unstable.go
    unstable.gocode
    unstable.gotools
    jq
    unstable.kubernetes
    python
    unstable.protobuf
    unstable.rustc
    terraform_0_10
    universal-ctags
    python36Packages.yamllint

    # Haskell.
    unstable.cabal2nix
    unstable.cabal-install
    unstable.nix-prefetch-git
    unstable.stack

    # System tools.
    file
    gnupg
    gzip
    openssl
    unstable.patchelf
    vagrant
    virtmanager

    # Interface.
    unstable.ripgrep
    vimHugeX
    tmux

    # Utilities.
    htop
    pass
    screenfetch
    tree
    unzip
  ];

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  nix.gc = {
    automatic = true;
    options = "-d";
  };

  networking = {
    hostName = "dev";
    extraHosts = ''
      172.17.0.2 matchbox.example.com 
      172.17.0.21 cluster.example.com 
      172.17.0.22 tectonic.example.com 
      172.17.0.21 node1.example.com 
      172.17.0.22 node2.example.com 
      172.17.0.23 node3.example.com
    '';
    nameservers = [ "8.8.8.8" "4.4.4.4" ];
  };

  nixpkgs.config = {
    allowUnfree = true;
  };

  programs = {
     bash = {
      enableCompletion = true;
      promptInit = ''PS1="\[\033[1;32m\][\u@\h:\W]\$\[\033[0m\] "'';
    };
    mtr.enable = true;
    gnupg.agent = { enable = true; enableSSHSupport = true; };
  };

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  services = {
    ddclient = {
      domain = "dev.diegs.ca";
      enable = true;
      username = "changeme";
      password = "changeme";
      protocol = "googledomains";
    };
    openssh.enable = true;
  };

  system = {
    autoUpgrade = {
      enable = true;
    };
    stateVersion = "17.09";
  };

  time.timeZone = "America/Los_Angeles";

  users.extraUsers.diegs = {
    isNormalUser = true;
    extraGroups = [ "docker" "libvirtd" "vboxusers" "wheel" ];
    password = "changeme";
    uid = 1000;
  };

  virtualisation = {
    docker.enable = true;
    libvirtd.enable = true;
    virtualbox.host.enable = true;
  };
}
