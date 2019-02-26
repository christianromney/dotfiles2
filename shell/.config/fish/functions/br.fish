function br -d "Remove a brew and delete from .brews"
  brew remove $argv &&  sed -i '' "/$argv/d" $HOME/.brews
end
