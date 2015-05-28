#! /bin/bash

###########################################
# Installing needed and backed up packages
###########################################
sudo pacman -Syyu zsh emacs vim unrar unzip ranger filezilla gimp tmux urxvt gnome-tweak-tool ncmpcpp i3 dmenu nodejs npm
sudo pacman -S $(< $HOME/dotfiles/pacman/repos.txt)
yaourt -S $(< $HOME/dotfiles/pacman/aur.txt)

###########################################
# Make symlinks for dotfiles
###########################################
ln -s $HOME/dotfiles/vim/.vimrc $HOME/.vimrc
ln -s $HOME/dotfiles/zsh/.zshrc $HOME/.zshrc
ln -s $HOME/dotfiles/.tmux.conf $HOME/.tmux.conf
ln -s $HOME/dotfiles/i3/.i3status.conf $HOME/.i3status.conf
cp -R $HOME/dotfiles/emacs/.emacs.d/ $HOME/.emacs.d
cp $HOME/dotfiles/emacs/.emacs $HOME/.emacs
ln -s $HOME/.Xdefaults $HOME/.Xdefaults

###########################################
# Install oh-my-zsh and set as default
###########################################
wget https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O - | sh && chsh -s /bin/zsh

###########################################
# Install homebrew if on a Mac
###########################################
if [ $(uname -s) == "Darwin"]; then
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi
