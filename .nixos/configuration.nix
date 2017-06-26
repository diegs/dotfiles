{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];
  boot = {
    loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot.enable = true;
    };
  };
  environment.interactiveShellInit = ". ${pkgs.gnome3.vte}/etc/profile.d/vte.sh";
  environment.systemPackages = with pkgs; [
    asciinema
    autorandr
    awscli
    cryptsetup
    dunst
    i3lock-color
    jsoncpp
    git
    glide
    gnupg
    gnumake
    go
    google-chrome
    google-cloud-sdk
    gotools
    htop
    jq
    keybase
    kubernetes
    lemonbar-xft
    libu2f-host
    lxappearance
    pass
    pinentry
    polybar
    powertop
    python
    python27Packages.bugwarrior
    ripgrep
    rofi
    rofi-pass
    rustStable.rustc
    rustStable.cargo
    screenfetch
    sutils
    sxhkd
    slack
    spotify
    taskwarrior
    timewarrior
    termite
    tree
    vanilla-dmz
    vimHugeX
    xclip
    xorg.xbacklight
    xss-lock
    universal-ctags
    unzip
    usbutils
    vagrant
    weechat
    wpa_supplicant_gui
    xdo
    xtitle
    yarn
    yubikey-neo-manager
  ];
  fonts = {
    fonts = with pkgs; [ corefonts font-awesome-ttf siji unifont ];
  };
  hardware = {
    opengl.driSupport32Bit = true;
    pulseaudio = {
      enable = true;
      support32Bit = true;
    };
  };
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };
  nix.gc = {
    automatic = true;
    options = "-d";
  };
  nixpkgs.config = {
    allowUnfree = true;
  };
  programs = {
    bash = {
      enableCompletion = true;
      promptInit = "PS1=\"\\u@\\h \\w% \"";
    };
    ssh = {
      extraConfig = "AddKeysToAgent yes";
    };
  };
  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };
  services = {
    acpid.enable = true;
    avahi = {
      enable = true;
      nssmdns = true;
    };
    journald.extraConfig = "SystemMaxUse=50M";
    pcscd.enable = true;
    printing = {
      enable = true;
      drivers = [ pkgs.hplip ];
    };
    redshift = {
      enable = true;
      latitude = "37.758750";
      longitude = "-122.416155";
    };
    unclutter-xfixes.enable = true;
    thermald.enable = true;
    udev.packages = with pkgs; [ libu2f-host ];
    xserver = {
      desktopManager.xterm.enable = false;
      displayManager.slim = {
        defaultUser = "diegs";
        enable = true;
        theme = pkgs.fetchurl {
          url = "https://github.com/edwtjo/nixos-black-theme/archive/v1.0.tar.gz";
          sha256 = "13bm7k3p6k7yq47nba08bn48cfv536k4ipnwwp1q1l2ydlp85r9d";
        };
      };
      enable = true;
      layout = "us";
      windowManager = {
        bspwm.enable = true;
      };
    };
  };
  system = {
    autoUpgrade = {
      enable = true;
    };
    stateVersion = "17.03";
  };
  time = {
    timeZone = "America/Los_Angeles";
    hardwareClockInLocalTime = true;
  };
  users.extraUsers.diegs = {
    isNormalUser = true;
    extraGroups = [ "docker" "rkt" "vboxusers" "wheel" ];
    password = "changeme";
    uid = 1000;
  };
  virtualisation = {
    docker.enable = true;
    rkt.enable = true;
    virtualbox.host.enable = true;
  };
}
