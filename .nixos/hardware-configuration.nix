# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, ... }:

{
  imports =
    [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  boot = {
    blacklistedKernelModules = [ "psmouse" ];
    extraModprobeConfig = ''
      options i915 modeset=1 enable_rc6=1 enable_fbc=1 enable_guc_loading=1 enable_guc_submission=1
      options snd_hda_intel power_save=1
    '';
    extraModulePackages = [ ];
    initrd = {
      availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
      luks.devices."crypted".device = "/dev/disk/by-uuid/5a4dbda6-a794-48bc-abb7-34a82c633ea4";
    };
    kernelModules = [ "intel_agp" "kvm-intel" ];
    # kernelParams = [ "pci=noaer" ];
  };

  fileSystems."/" =
    { device = "/dev/disk/by-label/nixos";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/nvme0n1p1";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-label/swap"; }
    ];

  hardware = {
    cpu.intel.updateMicrocode = true;
    enableAllFirmware = true;
  };
  networking = {
    hostName = "xps13";
    #networkmanager.enable = true;
    wireless = {
      enable = true;
      userControlled = true;
    };
  };
  nix.maxJobs = lib.mkDefault 4;
  powerManagement = {
    enable = true;
    cpuFreqGovernor = "powersave";
  };
  services.xserver = {
    videoDrivers = [ "intel" ];
    libinput = {
      clickMethod = "clickfinger";
      enable = true;
      disableWhileTyping = true;
      naturalScrolling = true;
    };
  };
}