# Exports
export ZSH=$HOME/.oh-my-zsh
export ZSH_THEME="kennethreitz"
export CASE_SENSITIVE="true"
export DISABLE_AUTO_UPDATE="false"

plugins=(git github cap gem brew)

export RUBYOPT="rubygems"
export ARCHFLAGS="-arch x86_64"
export GREP_OPTIONS="--color=auto"
export GREP_COLOR="7;33"
export EDITOR="vim"
export CLICOLOR="yes"
export REPORTTIME=30

if [[ -x $(which less) ]]; then
  export PAGER="less"
  export LESS="--ignore-case --LONG-PROMPT --QUIET --chop-long-lines -Sm --RAW-CONTROL-CHARS --quit-if-one-screen --no-init"
fi

# History control
export HISTIGNORE="&:exit:reset:clear"
export HISTSIZE=25000
export HISTFILE=~/.zsh_history
export SAVEHIST=5000
setopt INC_APPEND_HISTORY
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_REDUCE_BLANKS
setopt HIST_VERIFY
setopt RM_STAR_WAIT

source $ZSH/oh-my-zsh.sh

# Source scripts for specific tasks
source $HOME/.ec2/ec2.sh
source $HOME/.passwd
source $HOME/.path
source $HOME/.bash_aliases
source $HOME/.javarc
source $HOME/.oraclerc

# Ruby Version Manager
[[ -s $HOME/.rvm/scripts/rvm ]] && source $HOME/.rvm/scripts/rvm 
