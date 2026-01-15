# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a declarative macOS development environment using Nix with nix-darwin and home-manager. The configuration manages system packages, user tools, dotfiles, and macOS system preferences.

## Commands

```bash
# Apply configuration changes (primary command)
rebuild                                    # alias for darwin-rebuild switch --flake ~/dotfiles

# Update dependencies
nix flake update && rebuild               # update all inputs and apply
nix flake lock --update-input nixpkgs     # update specific input

# Rollback
darwin-rebuild switch --rollback          # revert to previous generation

# Search for packages
nix search nixpkgs <package-name>
```

## Architecture

```
flake.nix                    # Entry point - wires together nix-darwin, home-manager, nix-homebrew
├── hosts/darwin.nix         # macOS system config: homebrew casks/brews, system defaults (Dock, Finder, keyboard)
├── home/default.nix         # User config: CLI tools, zsh setup, shell aliases, program configs
└── config/                  # Dotfiles symlinked to ~/.config/
    ├── nvim/init.lua        # Neovim config (LazyVim-based)
    └── zed/                 # Zed editor settings
```

## Key Files

- **flake.nix**: Defines inputs (nixpkgs, nix-darwin, home-manager, nix-homebrew, mac-app-util) and wires modules together. Username/hostname configured here.
- **hosts/darwin.nix**: System packages, Homebrew packages (casks and brews), macOS defaults (Dock, Finder, NSGlobalDomain, trackpad). Homebrew `cleanup = "zap"` removes unlisted packages.
- **home/default.nix**: User packages (CLI tools, LSPs), shell configuration (zsh with oh-my-zsh), program configs (git, fzf, bat, eza, zoxide, atuin, direnv). Zsh theme selectable via `zshTheme` variable.

## Adding Packages

**Nix packages** (CLI tools): Add to `home.packages` in `home/default.nix`

**Homebrew formulas**: Add to `homebrew.brews` in `hosts/darwin.nix`

**Homebrew casks** (GUI apps): Add to `homebrew.casks` in `hosts/darwin.nix`

## Config File Management

Dotfiles in `config/` are symlinked via home-manager's `xdg.configFile`:
- `config/nvim/init.lua` → `~/.config/nvim/init.lua`
- `config/zed/*` → `~/.config/zed/*`

Changes to symlinked files apply immediately (no rebuild needed for editor configs).

## Shell Aliases

Key aliases are defined in `programs.zsh.shellAliases` in `home/default.nix`. Modern tool replacements are already aliased (e.g., `cat`→`bat`, `ls`→`eza`, `grep`→`rg`).

## Nix Syntax Notes

- `with pkgs;` allows referencing packages without `pkgs.` prefix
- `lib.mkMerge` / `lib.mkBefore` for ordering zsh init content
- `../config/` paths are relative to the nix file location
