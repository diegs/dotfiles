# .bashrc

if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

set -o vi

function _prepend_path() {
  if [ ! -d "$1" ]; then
    return
  fi
  if ! $( echo "$PATH" | tr ":" "\n" | grep -qx "$1" ) ; then
    export PATH="$1:$PATH"
  fi
}

_prepend_path "$HOME/.local/bin"
_prepend_path "$HOME/bin"
_prepend_path "$HOME/.cargo/bin"
_prepend_path "$HOME/.yarn/bin"

if [ -f ~/.nix-profile/etc/profile.d/nix.sh ]; then
  source ~/.nix-profile/etc/profile.d/nix.sh
fi

alias ls="ls --color=auto"
alias l="ls -lh"
alias ll="ls -lah"

alias vi=nvim
export EDITOR=nvim
export VISUAL=nvim
export GOPATH="$HOME"

# Base16 Shell
BASE16_SHELL="$HOME/.config/base16-shell/"
[ -n "$PS1" ] && [ -s "$BASE16_SHELL/profile_helper.sh" ] && eval "$("$BASE16_SHELL/profile_helper.sh")"

# Colors
RED="$(tput setaf 1)"
GREEN="$(tput setaf 2)"
YELLOW="$(tput setaf 3)"
BLUE="$(tput setaf 4)"
MAGENTA="$(tput setaf 5)"
CYAN="$(tput setaf 6)"
WHITE="$(tput setaf 7)"
GRAY="$(tput setaf 8)"
BOLD="$(tput bold)"
UNDERLINE="$(tput sgr 0 1)"
INVERT="$(tput sgr 1 0)"
NOCOLOR="$(tput sgr0)"
export CLICOLOR=1
alias grep='grep --color=auto'

# Bling.
eval $(dircolors ~/.dircolors)
local_username="diegs"
. ~/.prompt

# FZF.
bind -r "\C-j"
bind -r "\C-k"
export FZF_DEFAULT_OPTS="--bind=ctrl-y:accept,ctrl-k:up,ctrl-j:down"
export FZF_DEFAULT_COMMAND='rg --files --hidden'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
source ~/.nix-profile/share/fzf/completion.bash
source ~/.nix-profile/share/fzf/key-bindings.bash

# Local
if [ -f ~/.bash_local ]; then
  source ~/.bash_local
fi
