# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# stty -ixon
#
# # don't put duplicate lines or lines starting with space in the history.
# # See bash(1) for more options
# export HISTCONTROL=ignoredups:erasedups
# export HISTIGNORE="s:bg:fg:history"
#
# # append to the history file, don't overwrite it
# shopt -s histappend
#
# export HISTSIZE=
# export HISTFILESIZE=
#
# # Save and reload the history after each command finishes
# # export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"
#
# # check the window size after each command and, if necessary,
# # update the values of LINES and COLUMNS.
# shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
# [ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

PROMPT_COMMAND='__git_ps1 "${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u\[\033[00m\]@\[\033[01;34m\]\h\[\033[00m\]:\[\033[01;33m\]\W\[\033[00m\]" "\[\033[00m\]% " " [%s]"'

GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWSTASHSTATE=1
GIT_PS1_SHOWUNTRACKEDFILES=1
GIT_PS1_SHOWCOLORHINTS=1
GIT_PS1_DESCRIBE_STYLE="branch"
GIT_PS1_SHOWUPSTREAM="auto git"

if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

# If this is an xterm set the title to user@host:dir
# case "$TERM" in
# xterm*|rxvt*)
#     PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \W\a\]$PS1"
#     ;;
# *)
#     ;;
# esac

if [ -x /usr/bin/dircolors ]; then
  alias grep='grep --color=auto'
  alias fgrep='fgrep --color=auto'
  alias egrep='egrep --color=auto'

  alias ls='ls --color=auto'
  alias ll='ls -alF'
fi

# alias less="less -R"
# alias du="du -sch .[!.]* * | sort -h"
# alias yi-cfg="nix-shell -p haskellPackages.yi"

export BROWSER="google-chrome"
export EDITOR="vim"
export VISUAL="vim"
# set -o vi

if [ -f ~/.bashrc-local ]; then
  . ~/.bashrc-local
fi
