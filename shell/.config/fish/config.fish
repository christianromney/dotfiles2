set fish_greeting \n"|->>> Welcome back to "(hostname -s)", "(whoami)". <<<-|"\n

# add keys to ssh-agent
set keys "rsa" "ed25519"
for k in "$HOME/.ssh/id_"$keys
  ssh-add -K $k > /dev/null 2>&1
end

# prompt configuration
set SPACEFISH_PROMPT_ORDER user host dir git package conda node docker ruby rust julia aws exec_time time line_sep exit_code jobs char
set SPACEFISH_TIME_SHOW true
set SPACEFISH_TIME_PREFIX "("
set SPACEFISH_TIME_SUFFIX ") $SPACEFISH_PROMPT_DEFAULT_SUFFIX"
set SPACEFISH_USER_SHOW "needed"

set PATH $PATH $HOME/.cargo/bin /usr/local/anaconda3/bin $HOME/bin
eval (direnv hook fish)
source (conda info --root)/etc/fish/conf.d/conda.fish
