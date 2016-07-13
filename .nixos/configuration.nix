# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/vda";

  # networking.hostName = "nixos"; # Define your hostname. networking.wireless.enable = true; # Enables wireless support via wpa_supplicant.

  # Select internationalisation properties. i18n = {
  #   consoleFont = "Lat2-Terminus16"; consoleKeyMap = "us"; defaultLocale = "en_US.UTF-8";
  # };

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # List packages installed in system profile. To search by name, run: $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    git neovim silver-searcher stdenv tmux vcsh zsh haskellPackages.stack screenfetch irssi
  ];

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.permitRootLogin = "no";
  services.journald.extraConfig = "SystemMaxUse=50M";

  users.extraUsers.diegs = {
    isNormalUser = true;
    extraGroups = ["wheel"];
    shell = "/run/current-system/sw/bin/zsh";
  };
  security.pam.loginLimits = [
    { domain = "*"; type = "-"; item = "nofile"; value = "65535"; }
  ];
  security.sudo.wheelNeedsPassword = false;

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.03";

  programs.zsh.enable = true;

  system.autoUpgrade.enable = true;
  nix.gc.automatic = true;
  nix.gc.dates = "03:15";

  # services.logrotate.enable = true;
}
