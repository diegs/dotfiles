{
  description = "diegs home-manager flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixpkgs-22.11-darwin";
    flake-utils.url = "github:numtide/flake-utils";
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

  outputs = {self, nixpkgs, nixpkgs-stable, home-manager, emacs-overlay, ...}:
    let
      system = "aarch64-darwin";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        # overlays = [ emacs-overlay.overlay ];
      };
      pkgs-stable = import nixpkgs-stable {
        inherit system;
        config.allowUnfree = true;
      };
    in {
      homeConfigurations.diegs = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ ./home.nix ];
        extraSpecialArgs = { inherit pkgs-stable; };
      };
    };
}
