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
    '';
    extraModulePackages = [ ];
    initrd = {
      availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
      luks.devices."crypted".device = "/dev/disk/by-uuid/2ca960c9-fad6-4d6a-a0bc-b0719dbdaa9c";
    };
    kernelModules = [ "intel_agp" "kvm-intel" ];
  };

  fileSystems."/" =
    { device = "/dev/disk/by-label/nixos";
      fsType = "ext4";
    };


  fileSystems."/boot" =
    { device = "/dev/disk/by-label/UEFI";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-label/swap"; }
    ];

  hardware = {
    cpu.intel.updateMicrocode = true;
    enableAllFirmware = true;
  };
  networking.hostName = "xps13";
  nix.maxJobs = lib.mkDefault 4;
  powerManagement = {
    enable = true;
    cpuFreqGovernor = "powersave";
  };
  #services.kmscon = {
  #  enable = true;
  #  hwRender = true;
  #};
  services.xserver.videoDrivers = [ "intel" ];
}
