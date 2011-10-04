local return_code="%(?..%{$fg[red]%}%? ↵%{$reset_color%})"
local rvm_ruby='%{$fg[yellow]%}‹$(rvm-prompt i v g)›%{$reset_color%}'
RPS1="${return_code}"

GIT_PROMPT="\$(git_prompt_info)%{$reset_color%}"
PROMPT="╭─%{$fg_bold[cyan]%}%c$GIT_PROMPT$rvm_ruby 
╰─%B$%b "

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[red]%}➜"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[blue]%} %{$fg[yellow]%}✗%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[blue]%}"
