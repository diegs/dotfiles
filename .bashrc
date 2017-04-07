# If not running interactively, don't do anything
[ -z "$PS1" ] && return

stty -ixon
set -o vi

export HISTCONTROL=ignoredups:erasedups
export HISTIGNORE="s:bg:fg:history"
shopt -s histappend
export HISTSIZE=
export HISTFILESIZE=

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

PROMPT_COMMAND='__git_ps1 "${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u\[\033[00m\]@\[\033[01;34m\]\h\[\033[00m\]:\[\033[01;33m\]\W\[\033[00m\]" "\[\033[00m\]% " " [%s]"'
# PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"

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
  alias less="less -R"
fi

# alias yi-cfg="nix-shell -p haskellPackages.yi"

export BROWSER="google-chrome"
export EDITOR="vim"
export VISUAL="vim"

if [ -f ~/.bashrc-local ]; then
  . ~/.bashrc-local
fi

PATH="~/go/bin:$PATH"
