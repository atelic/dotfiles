#! /bin/bash

sudo pacman -Syu zsh emacs vim unrar unzip ranger filezilla gimp tmux urxvt gnome-tweak-tool ncmpcpp i3 dmenu
yaourt -S $(< $HOME/dotfiles/pacman/aur.txt)
ln -s $HOME/dotfiles/vim/.vimrc $HOME/.vimrc
ln -s $HOME/dotfiles/zsh/.zshrc $HOME/.zshrc
ln -s $HOME/dotfiles/i3/.i3status.conf $HOME/.i3status.conf
cp -R $HOME/dotfiles/emacs/.emacs.d/ $HOME/.emacs.d
cp $HOME/dotfiles/emacs/.emacs $HOME/.emacs
ln -s $HOME/.Xdefaults $HOME/.Xdefaults
wget https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O - | sh && chsh -s /bin/zsh
