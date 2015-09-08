HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
setopt HIST_IGNORE_DUPS
setopt appendhistory
unsetopt beep
zstyle :compinstall filename '/usr/local/google/home/diegs/.zshrc'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
zstyle ':completion:*' users root $USER
setopt completealiases
autoload -Uz compinit colors
compinit
colors
PROMPT="%{$fg[green]%}%n%{$reset_color%}@%{$fg[blue]%}%m%{$reset_color%}:%{$fg_no_bold[yellow]%}%1~%{$reset_color%}%# "

export KEYTIMEOUT=1
bindkey -v
bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey '^r' history-incremental-search-backward

setopt prompt_subst
autoload -Uz vcs_info
zstyle ':vcs_info:*' actionformats \
    '%F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{3}|%F{1}%a%F{5}]%f '
zstyle ':vcs_info:*' formats       \
    '%F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{5}]%f '
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{3}%r'
zstyle ':vcs_info:*' enable git cvs svn

# or use pre_cmd, see man zshcontrib
vcs_info_wrapper() {
  vcs_info
  if [ -n "$vcs_info_msg_0_" ]; then
    echo "%{$fg[grey]%}${vcs_info_msg_0_}%{$reset_color%}$del"
  fi
}
RPROMPT=$'$(vcs_info_wrapper)'

fancy-ctrl-z () {
  if [[ $#BUFFER -eq 0 ]]; then
    BUFFER="fg"
    zle accept-line
  else
    zle push-input
    zle clear-screen
  fi
}
zle -N fancy-ctrl-z
bindkey '^Z' fancy-ctrl-z

# Android.
export STAY_OFF_MY_LAWN=1
export ANDROID_USE_AMAKE=true
export ADB_VENDOR_KEYS="$HOME/etc/adb_key:"
# remove google3 JDK /usr/local/buildtools/java/jdk/bin from PATH on Goobuntu
export PATH=${PATH/\/usr\/local\/buildtools\/java\/jdk\/bin:/}

export BROWSER="google-chrome"
export EDITOR="vim"
export VISUAL="vim"
alias less="less -R"
alias du="du -sch .[!.]* * | sort -h"

if [[ $(uname -s) = 'Darwin' ]]; then
  alias ls="ls -G"
else
  alias ls="ls --color=auto"
fi

alias l="ls"

# Repo.
alias rs="repo sync -j100 -c -q"
alias rr="repo rebase"
alias rp="repo prune"
alias ruc="repo upload --cbr ."

[ -f ~/.zshrc-local ] && source ~/.zshrc-local