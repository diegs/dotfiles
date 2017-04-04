pkgs : {
  allowUnfree = true;
  vim = {
    python = true;
  };
  packageOverrides = pkgs: rec {
    myHaskellEnv = self.haskellPackages.ghcWithHoogle
                     (haskellPackages: with haskellPackages; [
                       xmonad xmonad-contrib xmonad-extras xmobar
                     ]);
    all = with pkgs; buildEnv {
      name = "all";
      paths = [
        dmenu git go htop irssi myHaskellEnv installed silver-searcher rustc screenFetch stack tmux xmobar
      ];
    };
  };
}
