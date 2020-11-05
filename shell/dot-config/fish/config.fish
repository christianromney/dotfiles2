set fish_greeting ">>> Welcome back, "(whoami)". <<<"
set EDITOR emacs

# Path
set PATH $HOME/.cargo/bin $HOME/bin $HOME/.emacs.d/bin $HOME/.jenv/bin  $PATH

# Nubank
if test -f $HOME/.nurc
  source $HOME/.nurc
end

# Python, Ruby, Rust, Direnv
status --is-interactive; and source (pyenv init -|psub) # python
status --is-interactive; and source (rbenv init -|psub) # ruby
status --is-interactive; and source (jenv init -|psub)  # jenv

eval (direnv hook fish)                                 # direnv

starship init fish | source                             # prompt
