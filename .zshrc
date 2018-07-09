source ~/.zsh_pluginsrc

HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory
unsetopt beep
bindkey -v
zstyle :compinstall filename '/home/dpontoriero/.zshrc'

autoload -Uz compinit
compinit

source '/home/dpontoriero/src/github.com/lyft/blessclient/lyftprofile' # bless ssh alias
source '/home/dpontoriero/src/github.com/lyft/awsaccess/awsaccess2.sh' # awsaccess
#export PS1="$(ps1_mfa_context)$PS1" # awsaccess

export EDITOR=vim
export GOPATH=/home/dpontoriero

BASE16_SHELL=$HOME/.config/base16-shell/
[ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"

alias l="ls -l"
alias antiup="antibody bundle < ~/.zsh_plugins > ~/.zsh_pluginsrc"
alias build_vbox="sudo /usr/lib/virtualbox/vboxdrv.sh setup"
alias vpn="cd ~/.vpn && ./vpn"

bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

bindkey '^y' autosuggest-accept
bindkey '^n' history-substring-search-up
bindkey '^p' history-substring-search-down
