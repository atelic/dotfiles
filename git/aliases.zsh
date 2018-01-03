# Use `hub` as our git wrapper:
#   http://defunkt.github.com/hub/
hub_path=$(which hub)
if (( $+commands[hub] ))
then
  alias git=$hub_path
  alias g=$hub_path
fi

# The rest of my fun git aliases
alias gl='git pull --prune'
alias glog="git log --graph --pretty=format:'%Cred%h%Creset %an: %s - %Creset %C(yellow)%d%Creset %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"
alias gp='git push origin HEAD'

if (( $+commands[diff-so-fancy] ))
then
  alias gd='git diff'
  alias gdc='git diff --color --cached'
else
  # Remove `+` and `-` from start of diff lines; just rely upon color.
  alias gd='git diff --color | sed "s/^\([^-+ ]*\)[-+ ]/\\1/" | less -r'
  alias gdc='git diff --color --cached | sed "s/^\([^-+ ]*\)[-+ ]/\\1/" | less -r'
fi

alias gc='git commit'
alias gca='git commit -a'
alias gco='git checkout'
alias gcb='git copy-branch-name'
alias gb='git branch'
alias gs='git status -sb' # upgrade your git if -sb breaks for you. it's fun.
alias gac='git add -A && git commit -m'
alias gst='git status'
alias gam='git add $(git ls-files --modified)'

function force {
  if [[  ! -z $1 ]]; then
    echo "Please supply a remote..."
    return 1
  fi
  remote="$1"
  BRANCH=$(git symbolic-ref --short -q HEAD)
  dest=${2:-"$BRANCH"}
  git push --force-with-lease "$remote" "$BRANCH"
}
