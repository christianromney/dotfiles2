#!/bin/bash
for d in $(pwd)/*; do
  if [ -d $d ]; then
    stow -t $HOME $(basename $d)
  fi
done
