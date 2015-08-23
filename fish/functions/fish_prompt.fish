function fish_prompt --description "Display the prompt"

  # Save our status
  set -l last_status $status
  set -l last_status_string ""
  if test $last_status -ne 0
    set last_status_string " [$last_status]"
  end

  # Just calculate this once, to save a few cycles when displaying the prompt
  if not set -q __fish_prompt_userhost
    set -g __fish_prompt_userhost
    if test \( -n "$SSH_CLIENT" \) -o \( -n "$SUDO_USER" \)
      set -l host (hostname|cut -d . -f 1)
      set -g __fish_prompt_userhost "$USER@$host "
    end
  end

  set -l color_cwd $fish_color_cwd
  set -l suffix '>'
  if test "$USER" = root
    set color_cwd $fish_color_cwd_root
    set suffix '#'
  end

  set -l normal (set_color normal)

  echo -n -s "$__fish_prompt_userhost" \
             (set_color $color_cwd) (prompt_pwd) \
             $normal (__fish_git_prompt) \
             (set_color $fish_color_status) $last_status_string \
             $normal " $suffix "
end
