set fish_greeting ">>> Welcome back, "(whoami)". <<<"
set EDITOR emacs
set PATH $HOME/bin $HOME/.cargo/bin $HOME/.emacs.d/bin $HOME/.jenv/bin $PATH

if test -f $HOME/.nurc
  bass source $HOME/.nurc
end

status --is-interactive; and source (pyenv init -|psub)
status --is-interactive; and source (rbenv init -|psub)
status --is-interactive; and source (jenv  init -|psub)

eval (direnv hook fish)

starship init fish | source
