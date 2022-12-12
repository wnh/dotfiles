. $HOME/.profile 
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd beep extendedglob nomatch PROMPT_SUBST
unsetopt AUTO_CD
bindkey -e

bindkey '\e[A' history-beginning-search-backward
bindkey '\e[B' history-search-forward

# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/wharding/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

autoload -U colors && colors
autoload -U promptinit && promptinit

export TERM=xterm-256color
export EDITOR=vim

alias ls='ls -G'
alias ll='ls -l'
alias lh='ls -lh'

alias g=git
alias gd='git diff'
alias gdc='git diff --cached'

today() { date +'%Y%m%d'; }
now() { date +"%Y%m%dT%H%M"; }
nownow() { date +"%Y%m%dT%H%M%S"; }


export DEFAULT_PROMPT="$PROMPT"
PROMPT="%{$fg[green]%}\$(swd)\$(gitst)%#%{$reset_color%} "

export PATH=$PATH:$HOME/bin

# Load NVM if it exists
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"

export ANSIBLE_NOCOWS=1
