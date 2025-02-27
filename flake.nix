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
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-darwin = {
      url = "github:c4710n/nix-darwin-emacs";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    ghostty.url = "github:ghostty-org/ghostty";
    ghostty-hm.url = "github:clo4/ghostty-hm-module";
  };

  outputs = inputs@{ self, darwin, home-manager, nixpkgs, emacs-overlay, emacs-darwin, ghostty, ghostty-hm }:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      overlays = [ emacs-overlay.overlay ];
      config = {
        allowUnfree = true;
      };
    };
  in {
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
             home-manager.extraSpecialArgs = { inherit ghostty; };
           }
         ghostty-hm.homeModules.default
       ];
       specialArgs = { inherit inputs; };
     };
    homeConfigurations."dpontoriero@5b45ab8-lcedt" = home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      modules = [
        ./home.nix
        ./linux.nix
        ghostty-hm.homeModules.default
      ];
      extraSpecialArgs = { inherit ghostty; };
    };
  };
}
