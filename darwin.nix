{ inputs, config, pkgs, ... }:
let
  emacs = (pkgs.emacsWithPackagesFromUsePackage {
    config = ./emacs.el;
    defaultInitFile = true;
    package = pkgs.emacs30;
    alwaysEnsure = true;
    extraEmacsPackages = epkgs: [
      epkgs.treesit-grammars.with-all-grammars
    ];
  });
in {
  environment.systemPackages = [
    # emacs
  ];

  # fonts.packages = [ pkgs.monaspace ];

  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = false;
      cleanup = "zap";
      upgrade = true;
    };
    brews = [
      "openconnect"
    ];
    casks = [
      "1password"
      "1password-cli"
      "scroll-reverser"
    ];
    global = {
      autoUpdate = false;
    };
  };

  services = {
    emacs = {
      enable = true;
      package = emacs;
      additionalPath = [ "/etc/profiles/per-user/dpontoriero/bin" ];
    };
    # Auto upgrade nix package and the daemon service.
    nix-daemon.enable = true;
  };

  # nix.package = pkgs.nix;
  nix.optimise.automatic = true;
  nix.settings.trusted-users = [ "root" "%admin" ];

  nixpkgs = {
    config = {
      allowUnfree = true;
    };
    overlays = [
      inputs.emacs-overlay.overlays.package
      inputs.emacs-darwin.overlays.emacs
    ];
  };

  # Necessary for using flakes on this system.
  nix.settings.experimental-features = "nix-command flakes";

  # Create /etc/zshrc that loads the nix-darwin environment.
  programs.zsh.enable = true;  # default shell on catalina
  # programs.fish.enable = true;

  # Set Git commit hash for darwin-version.
  system.configurationRevision = inputs.self.rev or inputs.self.dirtyRev or null;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "aarch64-darwin";

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

  security.pam.enableSudoTouchIdAuth = true;
}
