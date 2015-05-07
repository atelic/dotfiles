Dotfiles
=======

This repo serves as a backup for a number of my dotfiles as well as a compilation of commands and programs that I find myself running/needing when installing a new linux system

![Screenshot](/../master/screenshots/term_desktop.png?raw=true "Current system")

### Arch Linux post install

#### Installing needed packages
sudo pacman -Syu && \
sudo pacman -S unrar unzip ranger vim emacs geary corebird filezilla gimp inkscape scribus vlc eclipse terminator gnome-tweak-tool ncmpcpp irssi git i3 i3-status dmenu
yaourt spotify google-chrome i3-gap-git phpstorm android-studio dropbox

#### Dotfiles and configs
cd && git clone https://github.com/barbour-em/dotfiles.git
sh ~/dotfiles/makesymlinks.sh
OR
ln -s ~/dotfiles/vim/.vimrc . && \
ln -s ~/dotfiles/vim/.zshrc . && \
ln -s ~/dotfiles/i3/.i3status.conf . && \
cp .config/terminator/config .config/terminator/config.bak && ln -s ~/dotfiles/terminator/config .config/terminator/config

vim .vimrc and run :PluginInstall
### Web server fun
```
git config core.fileMode false
sudo -i
cd /opt && wget http://jaist.dl.sourceforge.net/project/xampp/XAMPP%20Linux/1.8.3/xampp-linux-1.8.3-4 installer.run
chmod +x xampp-linux-1.8.3-4-installer.run && ./xampp-linux-1.8.3-4-installer.run
ln -s ~/BeerLamp /opt/lampp/htdocs
chmod 775 -R /opt/lampp/htdocs
/opt/lampp/lampp restart
```

##### Reminder about mounting USB:
```
sudo -i
fdisk -l
mkdir /mnt/sdb1
vim /etc/fstab
```
```
/dev/sdb1       /mnt/sdb1           vfat    defaults        0       0
```
```
mount -a OR mount /dev/sdb1
```

#### Oh-my-zsh
pacman -S zsh && \
wget https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O - | sh
chsh -s /bin/zsh
