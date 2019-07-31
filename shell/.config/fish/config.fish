set fish_greeting \n"|->>> Welcome back to "(hostname -s)", "(whoami)". <<<-|"\n

# add keys to ssh-agent
set keys "rsa" "ed25519"
for k in "$HOME/.ssh/id_"$keys
  ssh-add -K $k > /dev/null 2>&1
end

set PATH $PATH $HOME/.cargo/bin /usr/local/anaconda3/bin

eval (direnv hook fish)

source (conda info --root)/etc/fish/conf.d/conda.fish
