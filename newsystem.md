A compilation of commands and programs that I find myself running/needing when installing a new linux system

### Fedora and RHEL Systems:

#### RPMFusion repos
su -c 'yum localinstall --nogpgcheck http://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm http://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm'

#### Fedy installer for nice and easy config
su -c "curl https://satya164.github.io/fedy/fedy-installer -o fedy-installer && chmod +x fedy-installer && ./fedy-installer"

Run fedy and fix fonts and install skype and sublime. May need to do first to add sudo to user

#### Dropbox
cd ~ && wget -O - "https://www.dropbox.com/download?plat=lnx.x86_64" | tar xzf -
~/.dropbox-dist/dropboxd

#### Mass install of packages
sudo yum update -y

sudo yum install -y unrar unzip ranger vim emacs geary corebird filezilla gimp inkscape scribus vlc eclipse terminator gnome-tweak-tool ncmpcpp irssi git

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

su -
cd /opt && wget http://jaist.dl.sourceforge.net/project/xampp/XAMPP%20Linux/1.8.3/xampp-linux-1.8.3-4-installer.run
chmod +x  xampp-linux-1.8.3-4-installer.run && ./xampp-linux-1.8.3-4-installer.run

ln -s ~/directory /opt/lampp/htdocs
chmod 775 -R /opt/lampp/htdocs && setenforce 0
/opt/lampp/lampp restart
Cry

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
Install phpstorm and android-studio from jetbeans online
