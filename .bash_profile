# .bash_profile

# Source global definitions
if [ -f /etc/bashrc ]; then
  . /etc/bashrc
fi

HISTCONTROL=ignoreboth:erasedups
HISTIGNORE='?:??'
HISTFILESIZE=1000000000
HISTSIZE=1000000

shopt -s histappend
shopt -s checkwinsize
shopt -s cdspell
shopt -s cmdhist
shopt -s no_empty_cmd_completion

for option in autocd globstar; do
  shopt -s "$option" 2> /dev/null
done

set -o noclobber
set -o vi

function _prepend_path() {
  if ! $( echo "$PATH" | tr ":" "\n" | grep -qx "$1" ) ; then
    export PATH="$1:$PATH"
  fi
}

[ -d /usr/local/go/bin ] && _prepend_path "/usr/local/go/bin"
[ -d $HOME/.local/bin ] && _prepend_path "$HOME/.local/bin"
[ -d $HOME/bin ] && _prepend_path "$HOME/bin"
[ -d $HOME/.cargo/bin ] && _prepend_path "$HOME/.cargo/bin"

# Base16 Shell
BASE16_SHELL="$HOME/.config/base16-shell/"
[ -n "$PS1" ] && \
  [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
  eval "$("$BASE16_SHELL/profile_helper.sh")"

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

if [ -n "$DESKTOP_SESSION" ];then
  eval $(gnome-keyring-daemon --start)
  export SSH_AUTH_SOCK
fi

alias l="ls -lh"
alias ll="ls -lah"

eval $(dircolors ~/.dircolors)
local_username="diegs"
. ~/.prompt
alias vim=vimx
export EDITOR=vimx
export GOPATH=/home/diegs


# FZF.
bind -r "\C-j"
bind -r "\C-k"
export FZF_DEFAULT_OPTS="--bind=ctrl-y:accept,ctrl-k:up,ctrl-j:down"
export FZF_DEFAULT_COMMAND='rg --files --hidden'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
source /usr/share/fzf/shell/key-bindings.bash

# Lyft.
export LYFT_CODE_ROOT=/home/diegs/src/github.com/lyft
export WORKSPACE=$LYFT_CODE_ROOT
source '/home/diegs/src/github.com/lyft/blessclient/lyftprofile' # bless ssh alias

function safeterm {
  # Term nodes by AWS ID or IP

  for arg in "$@"
  do
    if [[ "$arg" =~ ^([0-9]+\.){3}[0-9]+$ ]]
    then
      addr="$arg"
    else
      addr="i-$arg.ln"
    fi

    ssh -o StrictHostKeyChecking=no "$addr" 'sudo shutdown -h now'
  done
}
