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
#source ~/.dotfiles/base16-shell/base16-default.dark.sh

parse_git_branch() {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\[\1\]/'
}

alias ls='ls -G'
alias ll='ls -l'
alias lh='ls -lh'

alias g=git
alias gd='git diff'
alias gdc='git diff --cached'
alias gk='gitk --all &!'

alias errorlog='tail -f /var/log/apache2/error.log | sed s/$/\\n/'
alias dtags=drupaltags

today() { date +'%Y%m%d'; }
now() { date +"%Y%m%dT%H%M"; }
nownow() { date +"%Y%m%dT%H%M%S"; }


guix_flag() {
if [ -n "$GUIX_ENVIRONMENT" ] ; then echo "[G]"; fi
}
export DEFAULT_PROMPT="$PROMPT"
PROMPT="%{$fg[green]%}\$(swd) %{$fg[cyan]%}\$(gitst)%{$fg[green]%}\$(guix_flag)%#%{$reset_color%} "
# RPROMPT="%{$fg[blue]%}\$(gitst)%{$reset_color%}"

export PATH=$PATH:~/.bin:~/local/bin
export MANPATH=/Users/wharding/.homebrew/share/man:$MANPATH

export PATH="$PATH:/opt/go/bin"

# setup nvm 
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" # This loads nvm


# THIS IS THE GREATEST!!!
export CDPATH=.:$HOME/work:$GOPATH/src/github.com:$HOME/src/github.com


# If there is no docker host configured make an attempt
dm() {
  DOCKER_RUNNING=$(docker-machine ls | awk 'NR > 1 && $1 == "default" {print $4}')
  if [[ "$DOCKER_RUNNING" == "Stopped" ]]; then
    docker-machine start default
  fi
  eval $(docker-machine env)
}

scratch() {
  docker run --rm -it -v $(pwd):/host wnh/scratch:latest /bin/bash
}

ip() {
  ifconfig en0 inet | tail -n 1 | awk '{print $2}'
}


export GETGIT_ROOT=$HOME/src


#export PATH=$PATH:/Users/wharding/src/github.com/wnh/acstatus/ENV/bin

# opam configuration
test -r /home/wharding/.opam/opam-init/init.zsh && . /home/wharding/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

alias todo="vim $HOME/TODO"

export PATH="$PATH:$HOME/local/Apps/apache-ant-1.10.7/bin:$HOME/local/Apps/apache-maven-3.6.3/bin"
export PATH="$PATH:$HOME/local/py_env/bin"

#setup nix
. /home/wharding/.nix-profile/etc/profile.d/nix.sh

export ANSIBLE_NOCOWS=1

export PLAN9=$HOME/local/plan9
export PATH=$PATH:$PLAN9/bin

export GUIX_LOCPATH="$HOME/.guix-profile/lib/locale"
GUIX_PROFILE="/home/wharding/.guix-profile"
. "$GUIX_PROFILE/etc/profile"

