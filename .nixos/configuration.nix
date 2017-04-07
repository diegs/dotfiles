# x260

{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
      # ./local.nix
    ];

#  boot.loader.grub = {
#    enable = true;
#    version = 2;
#    efiSupport = true;
#    device = "nodev";
#  };
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi = {
    canTouchEfiVariables = true;
    efiSysMountPoint = "/boot";
  };

  networking.hostName = "diegs-x260";
  networking.networkmanager.enable = true;

  time.timeZone = "America/Los_Angeles";
  time.hardwareClockInLocalTime = true;

  environment.systemPackages = with pkgs; [
    google-chrome networkmanagerapplet vim virtualbox
  ];
  nixpkgs.config.allowUnfree = true;

  system.autoUpgrade.enable = true;
  system.stateVersion = "17.03";

  nix.gc = {
    automatic = true;
    options = "-d";
  };

  hardware.pulseaudio.enable = true;
  services.acpid.enable = true;
  powerManagement.enable = true;
  services.printing.enable = true;
  services.journald.extraConfig = "SystemMaxUse=50M";
  services.thermald.enable = true;
  services.xserver = {
#    exportConfiguration = true;
    enable = true;
    layout = "us";
#    synaptics = {
#      enable = true;
#      twoFingerScroll = true;
#    };
    videoDrivers = [ "intel" ];
#    xkbOptions = "caps:super";
    desktopManager.gnome3.enable = true;
    displayManager.gdm.enable = true;
#    windowManager.xmonad = {
#      enable = true;
#      enableContribAndExtras = true;
#      extraPackages = haskellPackages: [
#        haskellPackages.xmonad-contrib 
#        haskellPackages.xmonad-extras 
#        haskellPackages.xmonad
#        haskellPackages.taffybar
#      ];
#    };
#    windowManager.default = "xmonad";
  };
 
#  fonts = {
#    enableFontDir = true;
#    fonts = with pkgs; [ fira-mono hack-font inconsolata ];
#  };

  virtualisation.virtualbox.host.enable = true;

  users.extraUsers.diegs = {
    isNormalUser = true;
    extraGroups = ["networkmanager" "vboxusers" "wheel"];
  };
  security.sudo.wheelNeedsPassword = false;

  programs.bash = {
    promptInit = "PS1=\"\\u@\\h \\w% \"";
    enableCompletion = true;
  };
}
