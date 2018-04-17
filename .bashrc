# If not running interactively, don't do anything
case $- in
  *i*) ;;
  *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=10000
HISTFILESIZE=10000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

set -o vi

export EDITOR="vim"
export VISUAL="vim"
export GOPATH="/home/diegs"

BASE16_SHELL=$HOME/.config/base16-shell/
[ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"

# Set SSH to use gpg-agent
unset SSH_AGENT_PID
if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
  export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"
fi

# Set GPG TTY
export GPG_TTY=$(tty)

# Make sure git commands always update the SSH TTY.
export GIT_SSH=/home/diegs/.git_ssh

# Set git prompt
trap 'echo -ne "\e]0;${BASH_COMMAND%% *}\007"' DEBUG
function show_name { 
  if [[ -n "$BASH_COMMAND" ]]; then
    echo -en "\e]0;${PWD}\007"
  fi 
}

THE_PS1="$PS1"
PROMPT_COMMAND='__git_ps1 "" "$ " "[%s] "; show_name'

GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWSTASHSTATE=1
GIT_PS1_SHOWUNTRACKEDFILES=1
GIT_PS1_SHOWCOLORHINTS=1
GIT_PS1_DESCRIBE_STYLE="branch"
GIT_PS1_SHOWUPSTREAM="auto git"

alias bzl="docker run -v /var/run/docker.sock:/var/run/docker.sock -v /home/diegs:/home/diegs -u 1000:100 -it --rm quay.io/coreos/core-services-builder:v16"
