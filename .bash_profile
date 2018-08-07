# .bash_profile

# Source global definitions
if [ -f /etc/bashrc ]; then
  . /etc/bashrc
fi

HISTCONTROL=ignoreboth:erasedups
HISTIGNORE='ls:bg:fg:history'
PROMPT_COMMAND='history -a'
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

[ -d $HOME/.local/bin ] && _prepend_path "$HOME/.local/bin"
[ -d $HOME/bin ] && _prepend_path "$HOME/bin"

# Base16 Shell
BASE16_SHELL="$HOME/.config/base16-shell/"
[ -n "$PS1" ] && \
  [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
  eval "$("$BASE16_SHELL/profile_helper.sh")"

#alias compile_vbox="/usr/lib/virtualbox/vboxdrv.sh setup"


if [ -e /home/dpontoriero/.nix-profile/etc/profile.d/nix.sh ]; then
  . /home/dpontoriero/.nix-profile/etc/profile.d/nix.sh
fi

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
local_username="dpontoriero"
. ~/.bash_prompt
#export PS1="\n\[\033[1;32m\][\u@\h:\W]\$\[\033[0m\] "
export EDITOR=vim
export GOPATH=/home/dpontoriero

export FZF_DEFAULT_COMMAND='rg --files --hidden'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
source /usr/share/fzf/shell/key-bindings.bash

source '/home/dpontoriero/src/github.com/lyft/blessclient/lyftprofile' # bless ssh alias
source '/home/dpontoriero/src/github.com/lyft/awsaccess/awsaccess2.sh' # awsaccess
export PS1="\$(ps1_mfa_context)$PS1" # awsaccess
