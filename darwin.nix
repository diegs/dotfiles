{ inputs, ... }:
{
  environment.systemPackages = [ ];

  # Let Determinate do its thing
  nix.enable = false;

  # Set Git commit hash for darwin-version.
  system.configurationRevision = inputs.self.rev or inputs.self.dirtyRev or null;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 6;

  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "aarch64-darwin";
  nixpkgs.config.allowUnfree = true;

  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = false;
      cleanup = "zap";
      upgrade = true;
    };
    brews = [ "colima" "python" ];
    taps = [ "jimeh/emacs-builds" ];
    casks = [
      "1password"
      "1password-cli"
      "docker"
      "font-jetbrains-mono"
      "ghostty"
      "scroll-reverser"
      "jimeh/emacs-builds/emacs-app"
    ];
    masApps = {
      "remarkable-desktop" = 1276493162;
    };
    global = {
      autoUpdate = false;
    };
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
