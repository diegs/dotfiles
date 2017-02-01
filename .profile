# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
# if [ -n "$BASH_VERSION" ]; then
#     # include .bashrc if it exists
#     if [ -f "$HOME/.bashrc" ]; then
# 	. "$HOME/.bashrc"
#     fi
# fi

if [ -d "/usr/games" ] ; then
    PATH="/usr/games:$PATH"
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

export IDEA_JDK=/usr/lib/jvm/java-8-openjdk-amd64
export STUDIO_JDK=/usr/lib/jvm/java-8-openjdk-amd64

source $HOME/.cargo/env
