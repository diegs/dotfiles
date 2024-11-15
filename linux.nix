{ pkgs, ... }:
{
  home = {
    username = "dpontoriero";
    homeDirectory = "/home/dpontoriero";
  };
  dconf.settings = {
    "org/gnome/terminal/legacy" = {
      default-show-menubar = false;
      tab-policy = "always";
      theme-variant = "system";
    };
  };
  programs = {
    bash.enable = true;
    home-manager.enable = true;
    gnome-shell = {
      enable = false;
      extensions = [
        { package = pkgs.gnomeExtensions.gtile; }
        { package = pkgs.gnomeExtensions.night-theme-switcher; }
        { package = pkgs.gnomeExtensions.steal-my-focus-window; }
      ];
    };
  };
  services = {
    emacs = {
      enable = true;
      startWithUserSession = "graphical";
      client.enable = true;
    };
  };

  targets.genericLinux.enable = true;

  systemd.user.services = {
    fix_webcam = {
      Unit = {
        Description = "Fix IR webcam";
      };
      Service = {
        Type = "oneshot";
        ExecStart = toString (
          pkgs.writeShellScript "fix_webcam" ''
            #!/usr/bin/env bash
            sudo rm -f /dev/video2
          ''
        );
      };
      Install.WantedBy = [ "default.target" ];
    };
  };

  systemd.user.timers = {
    fix_webcam = {
      Timer = {
        OnStartupSec = "1m";
        OnUnitActiveSec = "1m";
      };
      Install.WantedBy = [ "timers.target" ];
    };
  };
}
