{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  boot.loader = {
    efi.canTouchEfiVariables = true;
    systemd-boot.enable = true;
  };

  environment.systemPackages = with pkgs; [
#    autorandr
    dmenu
    i3status
    i3lock
    fwupd
    git
    glib_networking
    gnupg
    google-chrome
    hfsprogs
    mercurial
    networkmanagerapplet
    pcsctools
#    polybar
    slack
    sway
    xclip
    xss-lock
    xorg.xbacklight
    vim
    yubikey-personalization
  ];

  fonts = {
#    fontconfig.defaultFonts = {
#      monospace = [ "Cousine" ];
#      sansSerif = [ "Arimo" ];
#      serif = [ "Tinos" ];
#    };
    fonts = with pkgs; [ corefonts ];
#    fonts = with pkgs; [ corefonts envypn-font noto-fonts terminus_font ];
  };

  hardware = {
    opengl.driSupport32Bit = true;
    pulseaudio.enable = true;
  };

  networking = {
    networkmanager.enable = true;
  };

  nix.gc = {
    automatic = true;
    options = "-d";
  };
  nixpkgs.config.allowUnfree = true;

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
    journald.extraConfig = "SystemMaxUse=50M";
    ntp.enable = true;
    pcscd.enable = true;
    printing = {
      drivers = with pkgs; [ hplip ];
      enable = true;
    };
    udev.packages = with pkgs; [ libu2f-host ];
    xserver = {
      enable = true;
      layout = "us";
      #monitorSection = "DisplaySize 293 165";
      displayManager.gdm.enable = true;
      desktopManager.gnome3.enable = true;
    };
  };

  systemd.packages = with pkgs; [ fwupd ];
  systemd.targets."multi-user".conflicts = [ "getty@tty1.service" ];
  system = {
    autoUpgrade = {
      channel = "https://nixos.org/channels/nixos-unstable";
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
    uid = 1000;
    extraGroups = [ "docker" "networkmanager" "rkt" "vboxusers" "wheel" ];
    password = "changeme";
  };

  virtualisation = {
    docker.enable = true;
    rkt.enable = true;
    virtualbox.host.enable = true;
  };
}
