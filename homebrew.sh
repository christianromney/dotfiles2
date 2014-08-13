#!/bin/bash
brew list | diff -u -- brews.txt - | sed '1,5D' | awk '/^\-/' | sed 's/\-//' | while read b; do
  if [ -n "$b" ]; then
    brew install $b
  fi
done

brew cask list | diff -u -- casks.txt - | sed '1,5D' | awk '/^\-/' | sed 's/\-//' | while read c; do
  if [ -n "$c" ]; then
    brew cask install $c
  fi
done
