# Exports
export ZSH=$HOME/.oh-my-zsh
export ZSH_THEME="kennethreitz"
export CASE_SENSITIVE="true"
export DISABLE_AUTO_UPDATE="false"
export RUBYOPT="rubygems"
export ARCHFLAGS="-arch x86_64"
export GREP_OPTIONS="--color=auto"
export GREP_COLOR="7;33"
export EDITOR="vim"
export CLICOLOR="yes"
export REPORTTIME=30
export LESS="--ignore-case --LONG-PROMPT --QUIET --chop-long-lines -Sm --RAW-CONTROL-CHARS --quit-if-one-screen --no-init"
export PAGER=less
export LC_CTYPE=en_US.UTF-8

autoload -U url-quote-magic
zle -N self-insert url-quote-magic
bindkey "^[m" copy-prev-shell-word

# add a function path
fpath=($ZSH/functions $fpath)
for config_file ($ZSH/lib/*.zsh) source $config_file

# Load all of the plugins that were defined in ~/.zshrc
plugin=${plugin:=()}
plugins=(git github cap gem brew)
for plugin ($plugins) source $ZSH/plugins/$plugin/$plugin.plugin.zsh

# Load the theme
source "$ZSH/themes/$ZSH_THEME.zsh-theme"

# Miscellaneous options
setopt auto_name_dirs
setopt auto_pushd
setopt pushd_ignore_dups
setopt long_list_jobs

# History control
alias history="fc -l 1"
export HISTIGNORE="&:exit:reset:clear"
export HISTSIZE=25000
export HISTFILE=~/.zsh_history
export SAVEHIST=10000
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

# Source scripts for specific tasks
source $HOME/.aliases
source $HOME/.ec2/ec2.sh
source $HOME/.passwd
source $HOME/.path
source $HOME/.javarc
source $HOME/.oraclerc

# Ruby Version Manager
[[ -s $HOME/.rvm/scripts/rvm ]] && source $HOME/.rvm/scripts/rvm 
