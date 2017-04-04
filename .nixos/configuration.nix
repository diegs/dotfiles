{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.device = "nodev";

  networking.hostName = "nixos";
  time.timeZone = "America/Los_Angeles";
  time.hardwareClockInLocalTime = true;
  system.stateVersion = "16.09";
  system.autoUpgrade.enable = true;
  virtualisation.virtualbox.host.enable = true;
  nixpkgs.config.allowUnfree = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;

  nix.gc = {
    automatic = true;
    options = "-d";
  };

  environment.systemPackages = with pkgs; [
    google-chrome vim virtualbox wget
  ];

  services.journald.extraConfig = "SystemMaxUse=50M";
  services.xserver = {
    enable = true;
    # xrandrHeads = [ "DP-2" ];
    layout = "us";
    videoDrivers = [ "nvidia" ];
    exportConfiguration = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: [
        haskellPackages.xmonad-contrib 
        haskellPackages.xmonad-extras 
        haskellPackages.xmonad
      ];
    };
    windowManager.default = "xmonad";
  };
  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [ fira-mono hack-font inconsolata ];
  };

  users.extraUsers.diegs = {
    isNormalUser = true;
    extraGroups = ["vboxusers" "wheel"];
  };
  security.sudo.wheelNeedsPassword = false;

  programs.bash = {
    promptInit = "PS1=\"\\u@\\h \\w% \"";
    enableCompletion = true;
  };
}
