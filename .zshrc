# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

ZSH_THEME="fino"

# Set to this to use case-sensitive completion
CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

plugins=(extract sprunge gnu-utils rvm brew vagrant cap heroku lein node npm python pip)

source $ZSH/oh-my-zsh.sh

# BEGIN CUSTOMIZATIONS

# Exports
export ARCHFLAGS="-arch x86_64"
export GREP_OPTIONS="--color=auto"
export GREP_COLOR="7;33"
export EDITOR="vim"
export VIEWER="view"
export CLICOLOR="yes"
export REPORTTIME=30
export LESS="--ignore-case --LONG-PROMPT --QUIET --chop-long-lines -Sm --RAW-CONTROL-CHARS --quit-if-one-screen --no-init"
export PAGER=less
export LC_CTYPE=en_US.UTF-8
export HISTIGNORE="&:exit:reset:clear"
export HISTSIZE=25000
export HISTFILE=~/.zsh_history
export SAVEHIST=10000
export UNCRUSTIFY_CONFIG="/usr/local/Cellar/uncrustify/0.59/share/uncrustify/gnu-indent.cfg"

# For git-vendors
export vendors="PointSlope Sapient IBM"

# Shell options
setopt append_history
setopt extended_history
setopt inc_append_history
setopt hist_expire_dups_first
setopt hist_ignore_space
setopt hist_ignore_dups 
setopt hist_ignore_all_dups
setopt hist_reduce_blanks
setopt hist_verify
setopt share_history 
setopt rm_star_wait
setopt auto_pushd
setopt pushd_ignore_dups
setopt long_list_jobs

# These next two lines allow me to source any .javarc files that
# are found in a working directory I 'cd' into. This can be extremely 
# dangerous since I haven't yet added 'trusted file' functionality like rvm
# does. Use only if you understand the risks.
fpath=($ZSH_CUSTOM/funcs.zsh $fpath)
chpwd_functions=(${chpwd_functions[@]} "source_javarc")

# Source scripts for specific tasks
source $HOME/.aliases
source $HOME/.ec2/ec2.sh
source $HOME/.passwd
source $HOME/.oraclerc
source $HOME/.javarc
source $HOME/.path


sudo () { ( unset LD_LIBRARY_PATH DYLD_LIBRARY_PATH; exec command sudo $* ) }

# FASD
eval "$(fasd --init posix-alias zsh-hook)"

# Ruby Version Manager
[[ -s $HOME/.rvm/scripts/rvm ]] && source $HOME/.rvm/scripts/rvm 

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
