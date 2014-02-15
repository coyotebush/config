function fish_prompt
  if test -n "$SSH_CLIENT"
    echo -n (hostname|cut -d. -f1)' '
  end
  set_color $fish_color_cwd
  echo -n (prompt_pwd)
  set_color normal
  echo -n (__fish_git_prompt)
  set_color normal
  echo -n ' > '
end
