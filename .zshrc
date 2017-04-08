# PATHS
export ZSH=/Users/tees/.oh-my-zsh
export GOPATH=/Users/tees/Development/go/

#ZSH_THEME="robbyrusssel"

export EDITOR='emacs'
export GIT_EDITOR=vim

## ZSH SETTINGS ## 
ENABLE_CORRECTION="true"

## PLUGINS AND JUNK
plugins=(git, z)
source $ZSH/oh-my-zsh.sh

##  ALIASES ##
alias zshconfig="nvim ~/.zshrc"
alias subl="open -a /Applications/Sublime\ Text.app"
