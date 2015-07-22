#! /bin/sh

cd $HOME
if [ -d $HOME/dotfiles ]
then
    git clone https://github.com/barbour-em/dotfiles.git barbour-em-dotfiles && cd barbour-em-dotfiles
else
    git clone https://github.com/barbour-em/dotfiles.git dotfiles && cd dotfiles
fi

if [ $(uname -s) -eq "Darwin" ]
then
    git checkout mac-setup
fi

sh $PWD/bootstrap.sh




