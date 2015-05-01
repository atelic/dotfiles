#!/usr/bin/sh
HOME = /home/eric/

echo "starting to copy..."                                            &
cp $HOME/.config/terminator/config $HOME/dotfiles/terminator/config   &
cp $HOME/.i3/config $HOME/dotfiles/i3/config                          &
cp $HOME/.i3status.conf $HOME/dotfiles/.i3status.conf                 &
echo "done"

