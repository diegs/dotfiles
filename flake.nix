{
  description = "Example nix-darwin system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin = {
      url = "github:nix-darwin/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs, home-manager }:
  {
    # Build darwin flake using:
    # $ darwin-rebuild build --flake .#dpontoriero-mlt
    darwinConfigurations."dpontoriero-mlt" = nix-darwin.lib.darwinSystem {
      modules = [
        { system.primaryUser = "dpontoriero"; }
        ./darwin.nix
        home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.dpontoriero = import ./home.nix;
            users.users.dpontoriero.home = "/Users/dpontoriero";
            home-manager.extraSpecialArgs = { };
          }
      ];
      specialArgs = { inherit inputs; };
    };
    darwinConfigurations."marmish" = nix-darwin.lib.darwinSystem {
      modules = [
        { system.primaryUser = "diegs"; }
        ./darwin.nix
        home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.diegs = import ./home.nix;
            users.users.diegs.home = "/Users/diegs";
            home-manager.extraSpecialArgs = { };
          }
      ];
      specialArgs = { inherit inputs; };
    };
  };
}
