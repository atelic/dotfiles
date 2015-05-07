Dotfiles
=======

This repo serves as a backup for a number of my dotfiles as well as a compilation of commands and programs that I find myself running/needing when installing a new linux system

![Screenshot](/../master/screenshots/term_desktop.png?raw=true "Current system")

### Arch Linux post install

Antergos live USB -- Install as only OS on HDD with Gnome

#### Installing needed packages
sudo pacman -Syu 
sudo pacman -S unrar unzip ranger vim emacs geary corebird filezilla gimp inkscape scribus vlc eclipse terminator gnome-tweak-tool ncmpcpp irssi git i3 i3-status dmenu
yaourt spotify google-chrome i3-gap-git phpstorm android-studio dropbox

#### Projects
cd && git clone https://github.com/barbour-em/greenstogrounds.git

cd && git clone https://github.com/BeerLamp/BeerLamp.git && cd BeerLamp && git checkout development

#### Dotfiles and configs
cd && git clone https://github.com/barbour-em/dotfiles.git
sh ~/dotfiles/makesymlinks.sh
OR
ln -s ~/dotfiles/.vimrc .
ln -s ~/dotfiles/.zshrc .
ln -s ~/dotfiles/.i3status.conf .
cp .config/terminator/config .config/terminator/config.bak && ln -s ~/dotfiles/terminator/config .config/terminator/config

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


### Fedora and RHEL Systems:

#### RPMFusion repos
```
su -c 'yum localinstall --nogpgcheck http://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm http://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm'
```

#### Fedy installer for nice and easy config
```
su -c "curl https://satya164.github.io/fedy/fedy-installer -o fedy-installer && chmod +x fedy-installer && ./fedy-installer"
```

Run fedy and fix fonts and install skype and sublime. May need to do first to add sudo to user

#### Dropbox
```
cd ~ && wget -O - "https://www.dropbox.com/download?plat=lnx.x86_64" | tar xzf -
``` 
```
~/.dropbox-dist/dropboxd
```

#### Mass install of packages
```
sudo yum update -y

sudo yum install -y unrar unzip ranger vim emacs geary corebird filezilla gimp inkscape scribus vlc eclipse terminator gnome-tweak-tool ncmpcpp irssi git
```

git clone git://github.com/VitaliyRodnenko/geeknote.git .evernote && cd .evernote
sudo python setup.py install
geeknote login

#### Google chrome
su -

cat << EOF > /etc/yum.repos.d/google-chrome.repo
[google-chrome]
name=google-chrome - \$basearch
baseurl=http://dl.google.com/linux/chrome/rpm/stable/\$basearch
enabled=1
gpgcheck=1
gpgkey=https://dl-ssl.google.com/linux/linux_signing_key.pub
EOF

yum install google-chrome-stable

#### Spotify

sudo yum-config-manager --add-repo=http://negativo17.org/repos/fedora-spotify.repo && sudo yum -y install spotify-client

#### XAMPP

    *su -
    *cd /opt && wget http://jaist.dl.sourceforge.net/project/xampp/XAMPP%20Linux/1.8.3/xampp-linux-1.8.3-4-installer.run
    * chmod +x  xampp-linux-1.8.3-4-installer.run && ./xampp-linux-1.8.3-4-installer.run
    *ln -s ~/directory /opt/lampp/htdocs
    *chmod 775 -R /opt/lampp/htdocs && setenforce 0
    */opt/lampp/lampp restart
    *Cry becuase you're still using Apache. Come on Eric

#### Dotfiles

cp ~/Dropbox/dot/.zshrc ~ && cp ~/Dropbox/dot/.bashrc && ~/Dropbox/dot/.vimrc && ~/Dropbox/dot/.space.sh ~
cd && git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim

vim .vimrc and run :PluginInstall

wget https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O - | sh
chsh -s /bin/zsh

cpan Term::ExtendedColor
git clone git://github.com/trapd00r/ls--.git && cd ls--
perl Makefile.PL
make && su -c 'make install'
cp ls++.conf $HOME/.ls++.conf

#### IDEs
##### Android Studio:
    wget -O android-studio-ide.zip https://dl.google.com/dl/android/studio/ide-zips/1.1.0/android-studio-ide-135.1740770-linux.zip

