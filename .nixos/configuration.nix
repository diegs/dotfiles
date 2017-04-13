{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  boot.loader = {
    systemd-boot.enable = true;
    efi = {
      canTouchEfiVariables = true;
      efiSysMountPoint = "/boot";
    };
  };

  networking = {
    hostName = "x260";
    networkmanager.enable = true;
  };

  time = {
    timeZone = "America/Los_Angeles";
    hardwareClockInLocalTime = true;
  };

  environment.systemPackages = with pkgs; [
    autorandr dmenu i3status i3lock git google-chrome networkmanagerapplet polybar xclip xss-lock xorg.xbacklight vim virtualbox
  ];
  nixpkgs.config.allowUnfree = true;

  hardware = {
    cpu.intel.updateMicrocode = true;
    pulseaudio.enable = true;
  };
  virtualisation = {
    docker.enable = true;
    rkt.enable = true;
    virtualbox.host.enable = true;
  };
  services = {
    journald.extraConfig = "SystemMaxUse=50M";
    xserver = {
      enable = true;
      layout = "us";
      libinput = {
        clickMethod = "clickfinger";
        enable = true;
        naturalScrolling = true;
      };
#      synaptics = {
#        enable = true;
#        twoFingerScroll = true;
#        additionalOptions = ''
#          Option "VertScrollDelta" "-111"
#          Option "HorizScrollDelta" "-111"
#        '';
#      }; 
      xkbOptions = "caps:super";
      videoDrivers = [ "intel" ];
      windowManager.i3.enable = true;
    };
#    services.compton = {
#      enable = true;
#      fade = true;
#      inactiveOpacity = "0.9";
#      shadow = true;
#      fadeDelta = 4;
#      backend = "xrender";
#    };
  };
  fonts = {
    fonts = with pkgs; [ corefonts font-awesome-ttf hack-font noto-fonts terminus_font ];
    fontconfig.ultimate.enable = true;
  };

  users.extraUsers.diegs = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "docker" "networkmanager" "rkt" "vboxusers" "wheel" ];
  };
  security.sudo.wheelNeedsPassword = false;

  programs.bash = {
    enableCompletion = true;
    promptInit = "PS1=\"\\u@\\h \\w% \"";
  };

  system = {
    stateVersion = "17.03";
    autoUpgrade.enable = true;
  };
  nix.gc = {
    automatic = true;
    options = "-d";
  };
}
