if [ -d "/usr/games" ] ; then
    PATH="$PATH:/usr/games"
fi

if [ -e /usr/local/google/home/diegs/.nix-profile/etc/profile.d/nix.sh ]; then
  . /usr/local/google/home/diegs/.nix-profile/etc/profile.d/nix.sh
fi

if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

if [ -d "$HOME/.cabal/bin" ] ; then
    PATH="$HOME/.cabal/bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

export _JAVA_AWT_WM_NONREPARENTING=1
