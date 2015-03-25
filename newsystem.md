A compilation of commands and programs that I find myself running/needing when installing a new linux system

### Fedora and RHEL Systems:

su -c 'yum localinstall --nogpgcheck http://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm http://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm'

su -c "curl https://satya164.github.io/fedy/fedy-installer -o fedy-installer && chmod +x fedy-installer && ./fedy-installer"

run fedy and fix fonts and install skype and sublime. May need to do first to add sudo to user

cd ~ && wget -O - "https://www.dropbox.com/download?plat=lnx.x86_64" | tar xzf -
~/.dropbox-dist/dropboxd

sudo yum update -y

sudo yum install -y unrar unzip ranger vim emacs geary corebird filezilla gimp inkscape scribus vlc eclipse terminator

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

cp ~/Dropbox/dot/.zshrc ~ && cp ~/Dropbox/dot/.bashrc && ~/Dropbox/dot/.vimrc
cd && git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim

vim .vimrc and run :PluginInstall
