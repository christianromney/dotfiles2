set fish_greeting \n"|->>> Welcome back to "(hostname -s)", "(whoami)". <<<-|"\n

# add keys to ssh-agent
set keys "rsa" "ed25519"
for k in "$HOME/.ssh/id_"$keys
  ssh-add -K $k > /dev/null 2>&1
end

set EDITOR emacs

# prompt configuration
set SPACEFISH_PROMPT_ORDER time user host dir git package conda node docker ruby rust julia aws exec_time line_sep exit_code jobs char
set SPACEFISH_TIME_SHOW true
set SPACEFISH_TIME_SUFFIX " $SPACEFISH_PROMPT_DEFAULT_SUFFIX"
set SPACEFISH_USER_SHOW "needed"

# Path
set PATH $HOME/.cargo/bin $HOME/bin $HOME/.emacs.d/bin $PATH

# Python, Ruby, Rust, Direnv
status --is-interactive; and source (pyenv init -|psub)             # python
status --is-interactive; and source (rbenv init -|psub)             # ruby
set RUST_SRC_PATH (rustc --print sysroot)/lib/rustlib/src/rust/src  # rust
eval (direnv hook fish)                                             # direnv
