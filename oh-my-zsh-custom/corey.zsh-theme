# ZSH Theme - Preview: http://gyazo.com/8becc8a7ed5ab54a0262a470555c3eed.png
local return_code="%(?..%{$terminfo[bold]$fg[red]%}%? ↵%{$reset_color%})"

function hg_qtop() {
  if [ -d .hg ]; then
    echo "$ZSH_THEME_GIT_PROMPT_PREFIX$(hg qtop)$ZSH_THEME_GIT_PROMPT_SUFFIX"
  fi
}

local user_host='%{$fg[green]%}%n@%m%{$reset_color%}'
local host='%{$fg[green]%}%m%{$reset_color%}'
local current_dir='%{$terminfo[bold]$fg[blue]%}%~%{$reset_color%}'
local current_time='%{$fg[green]%}%T%{$reset_color%}'
#local rvm_ruby='%{$fg[red]%}‹$(rvm-prompt i v g)›%{$reset_color%}'
local vcs_branch='$(svn_prompt_info)$(git_prompt_info)$(hg_qtop)%{$reset_color%}'

PROMPT="${host}:${current_dir} ${vcs_branch}%B$%b "
RPS1="${return_code}"

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[yellow]%}‹"
ZSH_THEME_GIT_PROMPT_SUFFIX="› %{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY=" %{$fg[red]%}*%{$fg[green]%}"
ZSH_THEME_GIT_PROMPT_UNTRACKED=" %{$fg[red]%}!%{$fg[green]%}"
ZSH_THEME_GIT_PROMPT_CLEAN=""
