function bi -d "Install a brew and update .brews"
  echo "$argv" | tee -a $HOME/.brews | xargs brew install
end
