set fish_greeting \n"|->>> Welcome back, "(whoami)". <<<-|"\n
set EDITOR emacs

# Path
set PATH $HOME/.cargo/bin $HOME/bin $HOME/.emacs.d/bin $PATH

# Python, Ruby, Rust, Direnv
status --is-interactive; and source (pyenv init -|psub)             # python
status --is-interactive; and source (rbenv init -|psub)             # ruby

eval (direnv hook fish)                                             # direnv

starship init fish | source
