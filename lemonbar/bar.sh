#!/bin/zsh
# Bar is launched by piping a seperate script into it.
# ~/.cofnig/lemonbar/output.sh | ~/.config/lemonbar/bar.sh

fg=FFFFFF
font="dina"
lemonbar -g 1365x16 -f $font -F \#FF$fg
