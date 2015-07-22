Dotfiles
=======

This repo serves as a backup for a number of my dotfiles as well as a compilation of commands and programs that I find myself running/needing when installing a new nix system

### Quick install
```
curl https://raw.githubusercontent.com/barbour-em/dotfiles/mac-setup/install.sh | bash
```

If you don't like the idea of curling into your shell ...

### Manual Install

- `git clone https://github.com/barbour-em/dotfiles`
- Checkout to the `mac-setup` branch if you'd like
- Run `./bootstrap.sh` from the top level directory

### OSX Yosemite 10.10.3
![Screenshot](https://i.imgur.com/ETo4c0M.png "Tmux running")
![Screenshot](https://i.imgur.com/v7d6dGU.png "More tmux")
![Screenshot](https://i.imgur.com/xdQdmI8.png "Floating")


###### Reminder to self about mounting USB:
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
