export ZSH=/home/eric/.oh-my-zsh

ZSH_THEME="nanotech"

# Base16 Shell
BASE16_SHELL="$HOME/.config/base16-shell/base16-default.dark.sh"
[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL

DISABLE_AUTO_UPDATE="true"
ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"
DISABLE_UNTRACKED_FILES_DIRTY="true"
HIST_STAMPS="mm/dd/yyyy"
plugins=( git colored-man colorize )

export TERM="xterm-256color"

export PATH="/usr/lib64/qt-3.3/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin:/home/eric/.gem/ruby/1.8/bin:/opt/nginx/sbin:/home/eric/.local/bin:/home/eric/bin:/home/eric/.gem/ruby/1.8/bin:/opt/nginx/sbin:/usr/bin/core_perl"
export MANPATH="/usr/local/man:$MANPATH"

source $ZSH/oh-my-zsh.sh

export LANG=en_US.UTF-8
export ALTERNATE_EDITOR=""
export EDITOR=emacsclient
export TERM=xterm-256color
export ARCHFLAGS="-arch x86_64"
export HISTCONTROL="erasedups:ignoreboth"
export HISTFILESIZE=500000
export HISTSIZE=100000
export HISTIGNORE="&:[ ]*:e"

############
#Aliases
###########
alias google-chrome-stable='google-chrome-stable --force-device-scale-factor'
alias ncmpcpp='sh ~/bin/music.sh'
alias emacs='emacsclient -t'
alias busy='hexdump -C /dev/urandom|grep "ca fe"'
alias gh="curl -u 'barbour-em' https://api.github.com/user/repos -d '{name:$1}'"
alias ecc='emacsclient -c &'
alias skype='skype --disable-cleanlooks'
alias php='/opt/lampp/bin/php'
alias open='rifle'
alias skype-'skype --disable-cleanlooks'
alias update_repos='cd /home/eric/src/  && for i in ./*/; do (cd $i && git pull); done'
alias c="clear"
alias e="exit"
alias ..="cd .."
alias cd..='cd ..'
alias ls='ls --color --group-directories-first'
alias composer="sudo php composer.phar"
alias bc='bc -l'
alias vi='vim'
alias svi='sudo vim'
alias edit='emacsclient'
alias df='df -h'
alias apache='sudo /opt/lampp/lampp start'
alias en='geeknote'
alias ox='chmod o+x'
alias xclip='xclip -selection clipboard'
alias pacman='sudo pacman'
alias hdd="df -h | grep /dev/sda2"
alias lock='cd ~ && ./flock'
alias brightness='xbacklight -set'
## Status and complex scripts ##
alias batt='upower -i /org/freedesktop/UPower/devices/battery_BAT0 | grep -E "state|to\ full|percentage"'
alias clock='while true; do tput clear; date +"%H : %M" | figlet ; sleep 1; done'
alias speed-test=' wget -O /dev/null http://speedtest.wdc01.softlayer.com/downloads/test10.zip'
##############
#Scripts
##############
[ -z "$PS1" ] && return
alias dna='cd /home/eric/.script/fun && ./dna && cd'
alias pacscr='cd /home/eric/.script/fun && ./pacman && cd'
alias stats='cd /home/eric/.script/fun && ./fetch && cd'
alias pipes='cd /home/eric/.script/fun && ./pipes && cd'
alias bars='cd /home/eric/.script/fun && ./colorbars && cd'
alias scheme='cd /home/eric/.script/fun && ./colorscheme && cd'

up (){
 for i in $(seq ${1: -1});do
   cd ../
 done
}

## EXTRACT FUNCTION ##
extract () {
  if [ -f $1 ] ; then
      case $1 in
          *.tar.bz2)   tar xvjf $1    ;;
          *.tar.gz)    tar xvzf $1    ;;
          *.bz2)       bunzip2 $1     ;;
          *.rar)       rar x $1       ;;
          *.gz)        gunzip $1      ;;
          *.tar)       tar xvf $1     ;;
          *.tbz2)      tar xvjf $1    ;;
          *.tgz)       tar xvzf $1    ;;
          *.zip)       unzip $1       ;;
          *.Z)         uncompress $1  ;;
          *.7z)        7z x $1        ;;
          *)           echo "don't know how to extract '$1'..." ;;
      esac
  else
      echo "'$1' is not a valid file!"
  fi
}

#mkdir and follow into it
function mkcd {
  if [ ! -n "$1" ]; then
      echo "Enter a directory name"
  elif [ -d $1 ]; then
      echo "\`$1' already exists"
  else
      mkdir $1 && cd $1
  fi
}

# upload to ptpb.pw
pb () { curl -F "c=@${1:--}" https://ptpb.pw/ }

# add url of newly uploaded past to clipboard
pbx () { curl -sF "c=@${1:--}" -w "%{redirect_url}" 'https://ptpb.pw/?r=1' -o /dev/stderr | xsel -l /dev/null -b }

# uploads a screenshot and puts the url in the clipboard
pbs () {
  scrot /tmp/$$.png
  pbx /tmp/$$.png
}

pyclean() {
  find . -name "*.py[co]" -delete
  find . -name "__pycache__" -delete
}

PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"
