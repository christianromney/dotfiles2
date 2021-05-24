set fish_greeting ">>> Welcome back, "(whoami)". <<<"

# ssh
ls ~/.ssh/ | egrep -v '(config|known_hosts|\.pub)' | while read f
  ssh-add $f
end

set EDITOR emacs
set -x GPG_TTY (tty)

if test -f $HOME/.nurc
  bass source $HOME/.nurc
end

set PATH /usr/local/MacGPG2/bin $HOME/bin $HOME/.cargo/bin $HOME/.emacs.d/bin $HOME/.jenv/bin $PATH
set -g fish_user_paths "/usr/local/sbin" $fish_user_paths

status --is-interactive; and source (pyenv init -|psub)
status --is-interactive; and source (rbenv init -|psub)
status --is-interactive; and source (jenv  init -|psub)

eval (direnv hook fish)

starship init fish | source
