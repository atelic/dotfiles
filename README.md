# dotfiles

Declarative macOS development environment using Nix, nix-darwin, and home-manager.

## What's Included

- **System packages** via Nix and Homebrew (declaratively managed)
- **macOS defaults** (Dock, Finder, keyboard, trackpad)
- **Shell setup** (zsh with oh-my-zsh, modern CLI replacements)
- **Editor configs** (Neovim, Zed)
- **Developer tools** (LSPs, git tools, container tools)

## Prerequisites

- macOS (Apple Silicon)
- [Determinate Nix Installer](https://github.com/DeterminateSystems/nix-installer)

## Installation

```bash
# Install Nix (if not already installed)
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install

# Clone this repo
git clone https://github.com/yourusername/dotfiles ~/dotfiles
cd ~/dotfiles

# Update username/hostname in flake.nix, then apply
darwin-rebuild switch --flake ~/dotfiles

# Run post-install setup (secrets, auth, permissions)
./scripts/bootstrap.sh
```

## Usage

```bash
rebuild                    # Apply configuration changes
nix flake update && rebuild  # Update all packages
darwin-rebuild switch --rollback  # Rollback to previous state
```

## Structure

```
flake.nix           # Entry point, inputs, module wiring
hosts/darwin.nix    # System config, Homebrew packages, macOS defaults
home/default.nix    # User packages, shell config, program settings
scripts/
└── bootstrap.sh    # Post-rebuild setup (auth, secrets, permissions)
config/             # Dotfiles (symlinked to ~/.config/)
├── nvim/init.lua   # Neovim with LazyVim
├── zed/            # Zed editor settings
├── starship.toml   # Shell prompt
├── ghostty/        # Ghostty terminal
├── aerospace.toml  # Tiling window manager
├── lazygit/        # Git TUI
└── yazi/           # File manager
```

## Adding Packages

| Type | File | Example |
|------|------|---------|
| Nix package | `home/default.nix` | `home.packages = [ pkgs.ripgrep ];` |
| Homebrew formula | `hosts/darwin.nix` | `homebrew.brews = [ "nvm" ];` |
| Homebrew cask | `hosts/darwin.nix` | `homebrew.casks = [ "raycast" ];` |

## Documentation

See [GUIDE.md](GUIDE.md) for detailed documentation on architecture, tools, and customization.

## Resources

- [nix-darwin manual](https://daiderd.com/nix-darwin/manual/)
- [Home Manager options](https://nix-community.github.io/home-manager/options.html)
- [nixpkgs search](https://search.nixos.org/packages)
