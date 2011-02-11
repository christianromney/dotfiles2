# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
export ZSH_THEME="kennethreitz"

# Set to this to use case-sensitive completion
export CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
export DISABLE_AUTO_UPDATE="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git github cap gem brew)

source $ZSH/oh-my-zsh.sh

# Source scripts for specific tasks
source $HOME/.ec2/ec2.sh
source $HOME/.passwd
source $HOME/.path
source $HOME/.bash_aliases
source $HOME/.javarc
source $HOME/.oraclerc

# Exports
export RUBYOPT="rubygems"
export ARCHFLAGS='-arch x86_64'
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='7;33'
export EDITOR=vim
export CLICOLOR="yes"

# Ruby Version Manager
[[ -s $HOME/.rvm/scripts/rvm ]] && source $HOME/.rvm/scripts/rvm 
