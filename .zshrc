# add a function path
export ZSH=$HOME/.oh-my-zsh
fpath=($ZSH/functions $fpath)
for config_file ($ZSH/lib/*.zsh) source $config_file

plugin=${plugin:=(brew cap dirpersist git git-flow github npm rails3)}
for plugin ($plugins) fpath=($ZSH/plugins/$plugin $fpath)

# Url quote magic
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# Load and run compinit
autoload -U compinit
compinit -i

# Exports
export ZSH_THEME="bira"
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
export HISTIGNORE="&:exit:reset:clear"
export HISTSIZE=25000
export HISTFILE=~/.zsh_history
export SAVEHIST=10000

# Load the theme
source "$ZSH/themes/$ZSH_THEME.zsh-theme"

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

# Source scripts for specific tasks
alias history="fc -l 1"
source $HOME/.aliases
source $HOME/.ec2/ec2.sh
source $HOME/.passwd
source $HOME/.path
source $HOME/.javarc
source $HOME/.oraclerc

# Ruby Version Manager
[[ -s $HOME/.rvm/scripts/rvm ]] && source $HOME/.rvm/scripts/rvm 
