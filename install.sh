#!/usr/bin/env bash
set -euo pipefail

echo "Installing Homebrew utilities"
cat brewfile | xargs brew install
cat caskfile | xargs brew cask install

echo "Setting up configuration"
./homesync

echo "Installing Doom emacs"
git clone --depth 1 https://github.com/hlissner/doom-emacs $HOME/.emacs.d
$HOME/.emacs.d/bin/doom install

echo "Installing Homebrew pre-commit hook"
if [ ! -f ".git/hooks/pre-commit" ]; then
  cp git-pre-commit-hook .git/hooks/pre-commit
  chmod +x .git/hooks/pre-commit
fi
