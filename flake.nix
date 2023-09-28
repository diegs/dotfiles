{
  description = "diegs home-manager flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixpkgs-22.11-darwin";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
#    emacs-overlay = {
#      url = "github:nix-community/emacs-overlay";
#      inputs.nixpkgs.follows = "nixpkgs";
#      inputs.flake-utils.follows = "flake-utils";
#    };
  };

  outputs = {self, nixpkgs, nixpkgs-stable, home-manager, ...}:
    let
      system = "aarch64-darwin";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        # config.allowUnsupportedSystem = true;
        # overlays = [ emacs-overlay.overlay ];
      };
      pkgo = import nixpkgs-stable {
        inherit system;
        config.allowUnfree = true;
        # config.allowUnsupportedSystem = true;
      };
    in {
      homeConfigurations.diegs = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ ./home.nix ];
        extraSpecialArgs = { inherit pkgo; };
      };
    };
}
