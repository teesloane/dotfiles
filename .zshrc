
#
# User configuration sourced by interactive shells
#

# Define zim location
export ZIM_HOME=${ZDOTDIR:-${HOME}}/.zim

# Start zim
[[ -s ${ZIM_HOME}/init.zsh ]] && source ${ZIM_HOME}/init.zsh

source $HOME/.tools/z.sh
source $HOME/.aliases

export PATH=~/Development/flutter/bin:~/.nimble/bin:$PATH

# export JAVA_HOME=/Library/Java/JavaVirtualMachines/graalvm-ce-java11-19.3.1/Contents/Home
# export GRAALVM_HOME=/Library/Java/JavaVirtualMachines/graalvm-ce-java11-19.3.1/Contents/Home
export GRAALVM_HOME=/Library/Java/JavaVirtualMachines/graalvm-ce-java11-20.0.0/Contents/Home

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

alias fzf="fzf --preview 'bat --style=numbers --color=always {} | head -500'"

. ~/.z.sh
