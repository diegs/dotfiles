{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  boot.loader = {
    efi.canTouchEfiVariables = true;
    systemd-boot.enable = true;
  };

  environment = {
    gnome3.excludePackages = with pkgs.gnome3; [
      epiphany
      evolution
      totem
    ];
    systemPackages = with pkgs; [
      fwupd
      git
      gnumake
      go
      google-chrome
      mercurial
      slack
      spotify
      sway
      xwayland
      usbutils
      vagrant
      vimHugeX
      yubikey-neo-manager
    ];
  };

  fonts = {
    fonts = with pkgs; [ corefonts ];
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
    #dbus.packages = with pkgs; [ fwupd ];
    journald.extraConfig = "SystemMaxUse=50M";
    pcscd.enable = true;
    printing = {
      drivers = with pkgs; [ hplip ];
      enable = true;
    };
    udev.packages = with pkgs; [ libu2f-host ];
    xserver = {
      enable = true;
      layout = "us";
      libinput = {
        clickMethod = "clickfinger";
        enable = true;
        disableWhileTyping = true;
        naturalScrolling = true;
      };
      monitorSection = "DisplaySize 293 165";
      displayManager = {
        gdm.enable = true;
      };
      desktopManager = {
        gnome3.enable = true;
        xterm.enable = false;
      };
    };
  };

  systemd = {
    packages = with pkgs; [ fwupd ];
    targets."multi-user".conflicts = [ "getty@tty1.service" ];
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
