# EC2 API keys
if [ -d $HOME/.ec2 ]; then
  if [ -f $HOME/.ec2/ec2.bash ]; then
    source $HOME/.ec2/ec2.bash
  fi
fi  

# Other credentials
if [ -f $HOME/.passwd ]; then
  source $HOME/.passwd
fi

# Source scripts for specific tasks
source $HOME/.path
source $HOME/.node
source $HOME/.bash_aliases
source $HOME/.javarc
source $HOME/.oraclerc
source /usr/local/etc/bash_completion.d/*.bash

# Useful bash prompt 
PS1="\\[\033[0;32m\]\$(vcprompt)\[\033[0m\]\D{%a/%d-%b} [\[\033[0;33m\]\W\[\033[0m\]]$ "

# Rubygems
export RUBYOPT="rubygems"
export ARCHFLAGS='-arch x86_64'

# Grep 
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='7;33'

# Editors
export EDITOR=vim

# Bash History Control
export HISTCONTROL=erasedups
export HISTSIZE=5000
shopt -s histappend
PROMPT_COMMAND="history -a;$PROMPT_COMMAND"

# Terminal Colors 
export CLICOLOR="yes"
export LSCOLORS="gxcxcxdxbxegedabagacDx"

# Ruby Version Manager
if [[ -s $HOME/.rvm/scripts/rvm ]] ; then source $HOME/.rvm/scripts/rvm ; fi
[[ -r $rvm_path/scripts/completion ]] && source $rvm_path/scripts/completion
