#! /usr/bin/bash

# __author__ = Eric Barbour

###########################################
# Installing needed packages for Arch
###########################################
if [ $(lsb_release -is) == "Arch" ]
then

    echo "Welcome back!\n"
    echo "Installing your preferred and backed up packages..."
    # Reminder: Save packages with -Qqe > packages.txt
    # and yaourt -Qqe > aur.txt
    sudo pacman -Syyu $(cat pacman/packages.txt)
    yaourt -S $(< $HOME/dotfiles/pacman/aur.txt)
    echo "done"
fi

###########################################
# Back up old dots so no conflicts
###########################################
echo "Backing up existing dotfiles"

files=( ".vimrc" ".zshrc" ".tmux.conf" ".i3/config" ".emacs" )
for file in "${files[@]}"
do
  if [-f $HOME/$file ]
  then
      /bin/mv $HOME/$file $HOME/$file.backup
  fi
done

echo "done"

###########################################
# Make symlinks for dotfiles
###########################################
echo "Making symlinks for dotfiles..."

/bin/ln -s $HOME/dotfiles/vim/.vimrc $HOME/.vimrc
/bin/ln -s $HOME/dotfiles/zsh/.zshrc $HOME/.zshrc
/bin/ln -s $HOME/dotfiles/.tmux.conf $HOME/.tmux.conf
/bin/ln -s $HOME/dotfiles/i3/.i3status.conf $HOME/.i3status.conf
if [ ! -d $HOME/.i3 ]
then
    /bin/mkdir $HOME/.i3
fi

/bin/ln -s $HOME/dotfiles/i3/config $HOME/.i3/config

echo "done"

###########################################
# Install oh-my-zsh and set as default
###########################################
echo "Installing oh-my-zsh and setting as default..."
wget https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O - | sh && chsh -s /bin/zsh
echo "done"

###########################################
# Download and configure Emacs config
###########################################
if [[ -d $HOME/.emacs.d ]];
then
    /bin/mv $HOME/.emacs.d $HOME/.emacs.d.backup
fi
git clone https://github.com/barbour-em/my-emacs.git

/bin/cp -R my-emacs $HOME/.emacs.d/
echo "Emacs configuration installed. Run emacs to install missing packages"


###########################################
# Install homebrew if on a Mac
###########################################
if [[ $(uname -s) -eq "Darwin"]];
then
    echo "Installing homebrew..."
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    echo "done"
fi
