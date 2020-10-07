set fish_greeting \n"|->>> Welcome back to "(hostname -s)", "(whoami)". <<<-|"\n

# add keys to ssh-agent
set keys "rsa" "ed25519"
for k in "$HOME/.ssh/id_"$keys
  ssh-add -K $k > /dev/null 2>&1
end

set EDITOR emacs

# Path
set PATH $HOME/.cargo/bin $HOME/bin $HOME/.emacs.d/bin $PATH

# Python, Ruby, Rust, Direnv
status --is-interactive; and source (pyenv init -|psub)             # python
status --is-interactive; and source (rbenv init -|psub)             # ruby

eval (direnv hook fish)                                             # direnv

starship init fish | source
