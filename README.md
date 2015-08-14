# Dotfiles by Eric Barbour

Features:

* the `playbooks` folder contains Ansible that provisions a machine with programs and configs
* This keeps my Mac and Arch machine in sync with a playbook for each
* Zsh shell with oh_my_zsh and my .zshrc
* Vim provisioned with Vundle
* Emacs with my [repo](https://github.com/barbour-em/my-emacs)
* My scripts are synced in ~/bin/

## Setup for new Mac machine

1. Install XCode tools using `xcode-select --install`
2. Install [Brew](http://brew.sh/)

  ```bash
  ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"
  ```

3. Install Ansible using Brew `brew install ansible`
4. `~/dotfiles` must be this repo

  ```bash
  git clone https://github.com/barbour-em/dotfiles.git ~/dotfiles
  ```
5. Install [XQuartz](https://xquartz.macosforge.org/landing/)
6. Run the Ansible playbook for Mac

  ```bash
  cd ~/dotfiles/playbooks && ./launch
  ```

## Setup for new Arch machine

```bash
sudo pacman -Syu ansible
git clone https://github.com/palcu/dotfiles.git ~/dotfiles
cd ~/dotfiles/playbooks && ./launch
```

### TODOS

- [ ] Sublime support is bad
- [ ] Needs support for `yaourt`
- [ ] Still missing some files like `~/.gkt-2.0`
