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
      autoUpdate = true;
      upgrade = true;
    };
    casks = [
      "1password"
      "1password-cli"
      "antigravity-cli"
      "ghostty"
      "mos"
      "music-decoy"
    ];
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
    # macOS Global System Defaults
    # To reset to factory defaults via terminal:
    #   defaults delete -g KeyRepeat
    #   defaults delete -g InitialKeyRepeat
    # Or adjust via System Settings -> Keyboard sliders.
    # Factory defaults: KeyRepeat = 6 (~90ms), InitialKeyRepeat = 25 (~375ms).
    NSGlobalDomain = {
      KeyRepeat = 3; # Faster repeat rate (~45ms per character)
      InitialKeyRepeat = 20; # Moderate delay before repeat starts (~300ms)

      # Disable auto-substitution of quotes, dashes, and autocorrect in Cocoa apps
      NSAutomaticCapitalizationEnabled = false;
      NSAutomaticDashSubstitutionEnabled = false;
      NSAutomaticPeriodSubstitutionEnabled = false;
      NSAutomaticQuoteSubstitutionEnabled = false;
      NSAutomaticSpellingCorrectionEnabled = false;
    };
  };

  security.pam.services.sudo_local.touchIdAuth = true;
}
