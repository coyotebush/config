# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
export ZSH_THEME="agnoster"
DEFAULT_USER="corey"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want disable red dots displayed while waiting for completion
# DISABLE_COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(extract svn phing vi-mode zsh-syntax-highlighting history-substring-search)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
unsetopt sharehistory

alias sudo='nocorrect sudo'
alias lt='ls -lt'

EDITOR=vim
if type mvim >/dev/null; then
  EDITOR=mvim
elif type gvim >/dev/null; then
  EDITOR=myvim
fi
alias :e=$EDITOR
export EDITOR="$EDITOR -f"

alias :q=exit

ZSH_HIGHLIGHT_STYLES[globbing]='bold'

zle-keymap-select () {
  case $KEYMAP in
    vicmd) print -n '\e]12;gray\a';;
    viins | main) print -n '\e]12;green\a';;
  esac
}

if ! type open >/dev/null; then
  open () {
    for f; do xdg-open $f &!; done
  }
fi

if [ -f /usr/local/etc/profile.d/z.sh ]; then
  . /usr/local/etc/profile.d/z.sh
fi
