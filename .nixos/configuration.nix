{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];
  boot.loader = {
    efi.canTouchEfiVariables = true;
    systemd-boot.enable = true;
  };
  environment.systemPackages = with pkgs; [
    dmenu
    i3blocks
    i3-gaps
    i3lock
    jsoncpp
    git
    gnupg
    gnumake
    go
    google-chrome
    lemonbar
    libu2f-host
    lxappearance
    polybar
    rofi
    slack
    spotify
    steam
    termite
    tree
    vanilla-dmz
    vimHugeX
    xss-lock
    yabar
    yubikey-neo-manager
  ];
  fonts = {
    fonts = with pkgs; [ font-awesome-ttf siji unifont ];
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
    journald.extraConfig = "SystemMaxUse=50M";
    pcscd.enable = true;
    udev.packages = with pkgs; [ libu2f-host ];
    xserver = {
      enable = true;
      layout = "us";
      displayManager.lightdm.enable = true;
      windowManager.bspwm.enable = true;
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
    extraGroups = [ "docker" "networkmanager" "rkt" "vboxusers" "wheel" ];
    password = "changeme";
    uid = 1000;
  };
  virtualisation = {
    docker.enable = true;
    rkt.enable = true;
    virtualbox.host.enable = true;
  };
}
