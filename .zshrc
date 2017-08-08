# PATHS
export ZSH=~/.oh-my-zsh
export GOPATH=~/Development/go
export PATH=$PATH:/usr/local/go/bin

#ZSH_THEME="robbyrusssel"

export EDITOR='nvim'
export GIT_EDITOR=nvim

## ZSH SETTINGS ## 
ENABLE_CORRECTION="true"

# let tmux rename it's windows 
DISABLE_AUTO_TITLE="true"

## PLUGINS AND JUNK
plugins=(git z zsh-autosuggestions)
source $ZSH/oh-my-zsh.sh

##  ALIASES ##
alias zshconfig="nvim ~/.zshrc"
alias subl="open -a /Applications/Sublime\ Text.app"
alias rm=trash

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# tools and stuffs
alias ls=exa -bghHliS


### DEPS ###
# exa, neovim
