#!/bin/sh

# Define your battery device. Look up in '/sys/class/power_supply/' for a directory named 'BAT0' ( it also can be 'BAT1 or something else )
device='BAT0'
battery="$(cat /sys/class/power_supply/$device/capacity)%"

# Volume Status for alsa users
volume="$(amixer get Master | tail -1 | sed 's/.*\[\([0-9]*%\)\].*/\1/')"

# Define your preferred terminal
terminal='urxvt -e'

# How many spaces do you want before the battery status ?
spaces=15

# Automating the number of spaces
function auto_space
{
for ((i = 0; i <= $spaces; i++)); do
  printf ' '
done
}

# Menu Order.
menu_list="Emacs\nWeb\nTerm\nMusic\nVim\nHtop\nRanger\nScreenshot\n$(eval auto_space)Batt: $battery\n Vol: $volume"

# Dmenu Preferences
Dmenu="/usr/bin/dmenu -p 'Run:' -fn 'Terminess Powerline' -i -y 350 -x 70 -w 1141 -nb '#2d2d2d' -sb '#515151' -nf '#cccccc' -sf '#f99157'"

cmd=$(echo -e "$menu_list" | eval $Dmenu)

case $cmd in
  Vim)
    $terminal vim ;;
  Web)
    google-chrome-stable ;;
  Ranger)
    $terminal ranger ;;
  Htop)
    $terminal htop ;;
  Term)
    $terminal zsh ;;
  Emacs)
    $terminal emacs -nw ;;
  Music)
    $terminal ncmpcpp ;;
  Screenshot)
    scrot '%F--%I:%M:%S:%p--$wx$h--scrot.png' -e 'mv $f ~/.scrots/' && notify-send 'Scrot Saved !' ;;
esac
exit 0
