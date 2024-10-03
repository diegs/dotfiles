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
      enable = true;
      extensions = [
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
}
