source_javarc() {
  if [ "$PWD" != "$HOME" ]; then
    if [ -f "$PWD/.javarc" ]; then
      source "$PWD/.javarc"
      printenv | grep -i java_home
    fi
  fi
}

