# PATHS
#ZSH_THEME="robbyrusssel"

## ZSH SETTINGS ##
ENABLE_CORRECTION="false"

# let tmux rename it's windows
DISABLE_AUTO_TITLE="true"

## PLUGINS AND JUNK
plugins=(git z )
source $ZSH/oh-my-zsh.sh

##  ALIASES ##
alias zshconfig="nvim ~/.zshrc"
alias subl="open -a /Applications/Sublime\ Text.app"
alias rm=trash

# window manager
alias wms="brew services start chunkwm; brew services start skhd"
alias wmk="brew services stop chunkwm; brew services stop skhd"


## Enable some vimness
# ripped from https://dougblack.io/words/zsh-vi-mode.html

bindkey -v

bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey '^r' history-incremental-search-backward
export KEYTIMEOUT=1

## End vimness

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# tools and stuffs
alias ls=exa -bghHliS
alias cat=bat
setopt HIST_IGNORE_SPACE
alias jrnl=" jrnl"

### DEPS ###
export GOPATH=$HOME/Development/go                              # go
export PATH=~/Development/go/bin:$PATH                          # go bin

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh



export PATH=/Users/tees/.local/bin:$PATH
export PATH=$PATH:~/Development/flutter/bin
export PATH="/usr/local/opt/llvm/bin:$PATH"
