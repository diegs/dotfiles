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
    autorandr
    dmenu
    i3status
    i3lock
    git
    gnupg
    google-chrome
    hfsprogs
    mercurial
    networkmanagerapplet
    pcsctools
    polybar
    xclip
    xss-lock
    xorg.xbacklight
    vim
    virtualbox
    yubikey-personalization
  ];

  fonts = {
    fontconfig.defaultFonts = {
      monospace = [ "Cousine" ];
      sansSerif = [ "Arimo" ];
      serif = [ "Tinos" ];
    };
    fonts = with pkgs; [ corefonts envypn-font noto-fonts terminus_font ];
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
    udev.packages = with pkgs; [ libu2f-host ];
    xserver = {
      enable = true;
      layout = "us";
      libinput = {
        clickMethod = "clickfinger";
        enable = true;
        naturalScrolling = true;
      };
      xkbOptions = "caps:super";
      windowManager.i3.enable = true;
    };
    compton = {
      enable = true;
#      fade = true;
#      inactiveOpacity = "0.9";
#      shadow = true;
#      fadeDelta = 4;
      backend = "xrender";
    };
  };

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
  };

  virtualisation = {
    docker.enable = true;
    rkt.enable = true;
    virtualbox.host.enable = true;
  };
}
