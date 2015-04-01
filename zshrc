# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd beep extendedglob nomatch PROMPT_SUBST
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
source ~/.dotfiles/base16-shell/base16-default.dark.sh

parse_git_branch() {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\[\1\]/'
}

alias ls='ls --color=auto'
alias ll='ls -l'

alias g=git
alias gd='git diff'
alias gdc='git diff --cached'
alias gk='gitk --all &!'

alias errorlog='tail -f /var/log/apache2/error.log | sed s/$/\\n/'
alias dtags=drupaltags

today() { date +'%Y%m%d'; }
now() { date +"%Y%m%d-%H%M"; }
nownow() { date +"%Y%m%d-%H%M%S"; }



PROMPT="%{$fg[green]%}\$(swd) %{$fg[cyan]%}\$(gitst)%{$fg[green]%}%#%{$reset_color%} "
# RPROMPT="%{$fg[blue]%}\$(gitst)%{$reset_color%}"

