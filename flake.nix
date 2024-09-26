{
  description = "Darwin system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-darwin = {
      url = "github:c4710n/nix-darwin-emacs";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs = inputs@{ self, darwin, home-manager, nixpkgs, emacs, emacs-darwin }:
  {
    # Build darwin flake using:
    # $ darwin-rebuild build --flake .#dpontoriero-mlt
    darwinConfigurations."dpontoriero-mlt" = darwin.lib.darwinSystem {
      modules = [
        ./darwin.nix
        home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.dpontoriero = import ./home.nix;
            users.users.dpontoriero.home = "/Users/dpontoriero";
          }
      ];
      specialArgs = { inherit inputs; };
    };
  };
}
