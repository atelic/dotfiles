Dotfiles
=======

This repo serves as a backup for a number of my dotfiles as well as a compilation of commands and programs that I find myself running/needing when installing a new nix system

### Quick install
```
curl https://raw.githubusercontent.com/barbour-em/dotfiles/mac-setup/install.sh | bash
```

### OSX Yosemite 10.10.3
![Screenshot](https://i.imgur.com/ETo4c0M.png "Tmux running")
![Screenshot](https://i.imgur.com/v7d6dGU.png "More tmux")
![Screenshot](https://i.imgur.com/xdQdmI8.png "Floating")


##### Post install details can be found in bootstrap.sh

You can run `./bootstrap.sh` from the cloned directory

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
