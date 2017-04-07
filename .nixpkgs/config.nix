pkgs : {
  allowUnfree = true;
  vim = {
    python = true;
  };
  packageOverrides = super: let pkgs = super.pkgs; in with pkgs; rec {
    myHaskellEnv = haskellPackages.ghcWithHoogle
                     (haskellPackages: with haskellPackages; [
                       xmonad xmonad-contrib xmonad-extras taffybar
                     ]);
    all = pkgs.buildEnv {
      name = "all";
      paths = [
        dmenu git gnumake go gocode godef goimports htop irssi myHaskellEnv silver-searcher rustc screenfetch stack tmux tree vagrant
      ];
    };
  };
}
