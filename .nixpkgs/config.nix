pkgs : {
  vim = {
    python = true;
  };
  packageOverrides = super: let self = super.pkgs; in
  {
    myHaskellEnv = self.haskellPackages.ghcWithHoogle
                     (haskellPackages: with haskellPackages; [
                       xmonad xmonad-contrib xmonad-extras xmobar yi
                     ]);
  };
}
