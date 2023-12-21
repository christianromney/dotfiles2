set fish_greeting ">>> Welcome back, "(whoami)". <<<"

set PATH $HOME/bin $HOME/.config/emacs/bin /usr/local/opt/grep/libexec/gnubin /usr/local/MacGPG2/bin $HOME/.cargo/bin $HOME/.docker/bin $PATH
set -g fish_user_paths /usr/local/sbin $fish_user_paths

set EDITOR emacs

# ensure gpg-agent is running and add ssh keys quietly
gpgconf --launch gpg-agent
ssh-add -q

set -x GPG_TTY (tty)
set -x GPG_AGENT_INFO "~/.gnupg/S.gpg-agent:"(pgrep gpg-agent)":1"
set -gx HOMEBREW_GITHUB_API_TOKEN (security find-internet-password -s api.github.com -w)

if test -f $HOME/.nurc
    bass source $HOME/.nurc
end

# asdf general purpose environment manager
status --is-interactive; and source /usr/local/opt/asdf/libexec/asdf.fish

# conda python environment manager
# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
if test -f /usr/local/Caskroom/miniconda/base/bin/conda
    eval /usr/local/Caskroom/miniconda/base/bin/conda "shell.fish" hook $argv | source
end
# <<< conda initialize <<<

# prompt tool
starship init fish | source

# envionrment vars per directory
direnv hook fish | source
