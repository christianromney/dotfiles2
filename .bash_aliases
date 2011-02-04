# Standard tool aliases
alias ...="cd ../.."
alias ..="cd .."
alias cd...="cd ../.."
alias cd..="cd .."
alias df="df -h"
alias grep="egrep"
alias la="ls -a"
alias ll="ls -lah"
alias lll="ls -lah | less"
alias rf="rm -rf"
alias z="clear"

# Misc
alias realias="source ~/.bash_aliases"
alias flushdns="dscacheutil -flushcache"

# Programming
alias njs="rlwrap node-repl"
alias iol="rlwrap io"
alias drush="drush -v -r ~/src/ibm/drupal -l http://local.ncl.com:8888"

# VPN routing and IP configuration
alias iroute="sudo route -n add -net 10.5.0.0/16 -interface ppp0"
alias nroute="sudo route -n add -net 192.168.200 -interface ppp0"
alias routes="nroute && iroute"
alias ip="ifconfig | grep -e \"inet 167.*\" | cut -d' ' -f2"

# Proxy Support
alias proxyon="source ~/.proxies"
alias noproxy="source ~/.noproxy"
alias proxies="printenv | sort | grep proxy"
