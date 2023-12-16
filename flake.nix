{
  description = "diegs home-manager flake";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixpkgs-23.11-darwin";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {self, nixpkgs, nixpkgs-stable, home-manager, ...}:
    let
      system = "aarch64-darwin";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        # config.allowUnsupportedSystem = true;
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
