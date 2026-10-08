{
  description = "Example nix-darwin system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-26.05-darwin";
    nix-darwin = {
      url = "github:nix-darwin/nix-darwin/nix-darwin-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs, home-manager }:
  {
    # Build darwin flake using:
    # $ darwin-rebuild build --flake .#marmish
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
