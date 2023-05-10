set fish_greeting ">>> Welcome back, "(whoami)". <<<"

set PATH $HOME/bin $HOME/.config/emacs/bin /usr/local/opt/grep/libexec/gnubin /usr/local/MacGPG2/bin $HOME/.cargo/bin /usr/local/Cellar/gawk/5.2.1_1/bin $HOME/.jenv/bin $PATH
set -g fish_user_paths /usr/local/sbin $fish_user_paths

# ensure gpg-agent is running and add ssh keys quietly
gpgconf --launch gpg-agent
ssh-add -q

set EDITOR emacs
set -x GPG_TTY (tty)
set -x GPG_AGENT_INFO "~/.gnupg/S.gpg-agent:"(pgrep gpg-agent)":1"
set -gx HOMEBREW_GITHUB_API_TOKEN (security find-internet-password -s api.github.com -w)

if test -f $HOME/.nurc
    bass source $HOME/.nurc
end

status --is-interactive; and source (pyenv init -|psub)
status --is-interactive; and source (rbenv init -|psub)
status --is-interactive; and source /usr/local/opt/asdf/libexec/asdf.fish

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
if test -f /usr/local/Caskroom/miniconda/base/bin/conda
    eval /usr/local/Caskroom/miniconda/base/bin/conda "shell.fish" hook $argv | source
end
# <<< conda initialize <<<

starship init fish | source
eval (direnv hook fish)
