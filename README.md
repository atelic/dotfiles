Dotfiles
=======

This repo serves as a backup for a number of my dotfiles as well as a compilation of commands and programs that I find myself running/needing when installing a new linux system

![Screenshot](/../master/screenshots/term_desktop.png?raw=true "Current system")

![Info and colors](/../master/screenshots/info.png?raw=true "Info and colors")

### Arch Linux post install details can be found in setup.sh

### Reminder about XAMPP if you have to deal with it again
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
