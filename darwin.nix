{ inputs, config, pkgs, ... }:
{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =
    [
    ];

  # Let Determinate do its thing
  nix.enable = false;
  # Necessary for using flakes on this system.
  # nix.settings.experimental-features = "nix-command flakes";

  # Set Git commit hash for darwin-version.
  system.configurationRevision = inputs.self.rev or inputs.self.dirtyRev or null;
  system.primaryUser = "dpontoriero";

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 6;

  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "aarch64-darwin";
  nixpkgs.config.allowUnfree = true;

  programs = {
	  _1password.enable = true;
    _1password-gui.enable = true;
  };
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = false;
      cleanup = "zap";
      upgrade = true;
    };
    brews = [ "python" ];
	  taps = [ "jimeh/emacs-builds" ];
    casks = [ "ghostty" "scroll-reverser" "jimeh/emacs-builds/emacs-app" ];
    masApps = {
      "1Password for Safari" = 1569813296;
      "Raycast" = 6738274497;
	    "wipr-2" = 1662217862;
	    "remarkable-desktop" = 1276493162;
	    "save-to-reader" = 1640236961;
	  };
    global = { autoUpdate = false; };
  };
  system.defaults = {
    dock = {
      autohide = true;
      show-recents = false;
    };
    finder = {
      AppleShowAllExtensions = true;
      ShowPathbar = true;
      FXEnableExtensionChangeWarning = false;
    };
    menuExtraClock.Show24Hour = true;
  };

  security.pam.services.sudo_local.touchIdAuth = true;
}
