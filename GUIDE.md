# Nix Dotfiles Guide

This is a declarative, reproducible macOS development environment using Nix.

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                         flake.nix                                │
│                    (entry point + inputs)                        │
└──────────────────────────┬──────────────────────────────────────┘
                           │
           ┌───────────────┼───────────────┐
           │               │               │
           ▼               ▼               ▼
    ┌─────────────┐ ┌─────────────┐ ┌─────────────┐
    │ nix-darwin  │ │home-manager │ │nix-homebrew │
    │             │ │             │ │             │
    │ System-wide │ │ User-level  │ │ Brew pkgs   │
    │ macOS config│ │ dotfiles    │ │ declarative │
    └──────┬──────┘ └──────┬──────┘ └─────────────┘
           │               │
           ▼               ▼
    hosts/darwin.nix  home/default.nix
                           │
                           ▼
                    config/
                    ├── zed/
                    └── nvim/
```

## Key Concepts

### What is Nix?

Nix is a package manager that:

- Installs packages in isolation (`/nix/store/`)
- Never breaks existing packages when updating
- Allows rollbacks to any previous state
- Builds packages reproducibly from source definitions

### What is nix-darwin?

A Nix module that manages macOS system settings:

- System packages (available to all users)
- macOS defaults (Dock, Finder, keyboard)
- Homebrew packages (declaratively)
- System services

### What is home-manager?

A Nix module that manages user-level configuration:

- User packages (your CLI tools)
- Dotfiles (shell config, editor config)
- Program-specific settings (git, fzf, zoxide)

### What is a Flake?

A `flake.nix` is a standardized way to define Nix projects:

- Declares inputs (dependencies) with pinned versions
- Defines outputs (what the flake produces)
- Ensures reproducibility via `flake.lock`

## File Structure

```
~/dotfiles/
├── flake.nix           # Main entry point
│                       # - Declares inputs (nixpkgs, nix-darwin, home-manager)
│                       # - Wires everything together
│                       # - Sets username, hostname, system
│
├── flake.lock          # Pinned versions of all inputs
│                       # - Ensures reproducibility
│                       # - Updated with `nix flake update`
│
├── hosts/
│   └── darwin.nix      # macOS system configuration
│                       # - System packages (vim, git, curl)
│                       # - Homebrew packages (casks, formulas)
│                       # - macOS defaults (Dock, Finder, keyboard)
│                       # - Touch ID for sudo
│
├── home/
│   └── default.nix     # User environment configuration
│                       # - CLI tools (ripgrep, fd, bat, etc.)
│                       # - Shell setup (zsh, oh-my-zsh, aliases)
│                       # - Program configs (git, fzf, zoxide)
│                       # - Editor packages (neovim, LSPs)
│
├── config/
│   ├── zed/
│   │   ├── settings.json   # Zed editor settings
│   │   ├── keymap.json     # Vim keybindings
│   │   └── tasks.json      # Dev workflow tasks
│   │
│   ├── nvim/
│   │   └── init.lua        # Neovim config (LazyVim)
│   │
│   ├── starship.toml       # Shell prompt config
│   ├── aerospace.toml      # Tiling window manager
│   ├── ghostty/
│   │   └── config          # Ghostty terminal settings
│   ├── lazygit/
│   │   └── config.yml      # Git TUI settings
│   └── yazi/
│       ├── yazi.toml       # File manager settings
│       └── keymap.toml     # File manager keybindings
│
├── scripts/
│   └── bootstrap.sh        # Post-rebuild setup wizard
│
├── .gitignore          # Excludes secrets, build outputs
└── GUIDE.md            # This file
```

## How It Works

### 1. The Rebuild Process

When you run `rebuild` (alias for `darwin-rebuild switch --flake ~/dotfiles`):

```
1. Nix reads flake.nix
2. Downloads/builds all packages defined in:
   - hosts/darwin.nix (system packages)
   - home/default.nix (user packages)
3. Applies macOS system settings
4. Symlinks dotfiles to their destinations:
   - config/zed/* → ~/.config/zed/*
   - config/nvim/* → ~/.config/nvim/*
   - config/starship.toml → ~/.config/starship.toml
   - config/ghostty/* → ~/.config/ghostty/*
   - config/lazygit/* → ~/.config/lazygit/*
   - config/yazi/* → ~/.config/yazi/*
   - config/aerospace.toml → ~/.aerospace.toml
5. Updates shell environment
6. Creates a new "generation" (rollback point)
```

### 2. Package Installation

Packages come from three sources:

| Source            | Defined In         | Example                        |
| ----------------- | ------------------ | ------------------------------ |
| Nix (nixpkgs)     | `home/default.nix` | ripgrep, neovim, rust-analyzer |
| Homebrew formulas | `hosts/darwin.nix` | nvm                            |
| Homebrew casks    | `hosts/darwin.nix` | iTerm2, Discord                |

**Why both Nix and Homebrew?**

- Nix: Better for CLI tools, reproducible, cross-platform
- Homebrew casks: Required for some macOS GUI apps

### 3. Dotfile Management

Home-manager symlinks config files:

```nix
# In home/default.nix
xdg.configFile = {
  "zed/settings.json".source = ../config/zed/settings.json;
  "nvim/init.lua".source = ../config/nvim/init.lua;
};
```

This creates:

```
~/.config/zed/settings.json → ~/dotfiles/config/zed/settings.json
~/.config/nvim/init.lua → ~/dotfiles/config/nvim/init.lua
```

**Benefits:**

- Edit files in `~/dotfiles`, changes apply immediately
- Version control your configs
- Restore on any machine with one command

### 4. Shell Integration

Your shell config is generated from `home/default.nix`:

```nix
programs.zsh = {
  enable = true;
  shellAliases = { ... };
  initContent = "...";
  oh-my-zsh = { ... };
};
```

This generates `~/.zshrc` with:

- Oh-my-zsh plugins
- Aliases (ls→eza, cat→bat, etc.)
- Tool integrations (fzf, zoxide, atuin)
- Powerlevel10k theme loading

## Common Operations

### Rebuild After Changes

```bash
rebuild
# Or explicitly:
darwin-rebuild switch --flake ~/dotfiles
```

### Update All Packages

```bash
cd ~/dotfiles
nix flake update    # Updates flake.lock
rebuild             # Applies updates
```

### Update Specific Input

```bash
nix flake lock --update-input nixpkgs
rebuild
```

### Rollback to Previous Generation

```bash
# List generations
darwin-rebuild --list-generations

# Rollback
darwin-rebuild switch --rollback
```

### Search for Packages

```bash
# Search nixpkgs
nix search nixpkgs ripgrep

# Or use the web: https://search.nixos.org/packages
```

### Garbage Collection

```bash
# Remove old generations (keeps last 5)
sudo nix-collect-garbage -d

# Remove everything older than 7 days
sudo nix-collect-garbage --delete-older-than 7d
```

## Adding Packages

### Add a Nix Package

Edit `home/default.nix`:

```nix
home.packages = with pkgs; [
  # ... existing packages
  newpackage    # Add here
];
```

Then rebuild.

### Add a Homebrew Formula

Edit `hosts/darwin.nix`:

```nix
homebrew.brews = [
  "existingbrew"
  "newbrew"    # Add here
];
```

### Add a Homebrew Cask (GUI app)

Edit `hosts/darwin.nix`:

```nix
homebrew.casks = [
  "existing-app"
  "new-app"    # Add here
];
```

## Adding Editor Config

### Zed

Edit files in `config/zed/`:

- `settings.json` - Editor settings
- `keymap.json` - Keybindings
- `tasks.json` - Runnable tasks

Changes apply immediately (symlinked).

### Neovim

Edit `config/nvim/init.lua`:

- Add plugins to the `lazy.setup()` spec
- Add keymaps at the bottom
- Add autocommands as needed

Restart Neovim to apply changes.

## Adding Shell Aliases

Edit `home/default.nix`:

```nix
programs.zsh.shellAliases = {
  # ... existing aliases
  newalias = "some command";
};
```

Then rebuild.

## New Machine Setup

After cloning and running `darwin-rebuild switch`, run the bootstrap script:

```bash
./scripts/bootstrap.sh
```

The bootstrap script interactively guides you through:

1. **Secrets file** - Creates `~/.secrets` template for API keys
2. **GitHub CLI** - Runs `gh auth login`
3. **SSH keys** - Generates ed25519 key and shows public key for GitHub
4. **Neovim plugins** - Triggers LazyVim plugin installation
5. **GitHub Copilot** - Authenticates Copilot in Neovim
6. **Atuin sync** - Optional shell history cloud sync
7. **macOS permissions** - Reminds about Accessibility/Input Monitoring grants
8. **Raycast setup** - Instructions to replace Spotlight

## Secrets Management

Secrets are stored in `~/.secrets` (never committed):

```bash
# ~/.secrets
export ANTHROPIC_API_KEY="sk-..."
export OPENAI_API_KEY="sk-..."
export GITHUB_TOKEN="ghp_..."
```

This file is:

- Sourced automatically by your shell
- Gitignored
- Permissions set to 600 (owner only)

To add a new secret:

```bash
echo 'export NEW_SECRET="value"' >> ~/.secrets
source ~/.secrets
```

## Customizing macOS Settings

Edit `hosts/darwin.nix`:

```nix
system.defaults = {
  dock = {
    autohide = true;
    tilesize = 48;
    # ... more dock settings
  };
  finder = {
    AppleShowAllFiles = true;
    # ... more finder settings
  };
  NSGlobalDomain = {
    KeyRepeat = 2;
    # ... more global settings
  };
};
```

Find available options:

- `man configuration.nix` (some overlap)
- https://daiderd.com/nix-darwin/manual/

## Troubleshooting

### "error: file 'nixpkgs' was not found"

```bash
# Ensure you're in the dotfiles directory or specify the flake
darwin-rebuild switch --flake ~/dotfiles
```

### "collision between X and Y"

Two packages provide the same file. Solutions:

1. Remove one package
2. Use `home.packages = with pkgs; [ (package.override { ... }) ];` to configure
3. Set priority: `(lib.hiPrio package)`

### "attribute 'X' missing"

Package name might be different. Search for it:

```bash
nix search nixpkgs <name>
```

### Homebrew Package Not Installing

Check if it's a cask or formula:

- Formulas: `brews = [ "name" ];`
- Casks (GUI apps): `casks = [ "name" ];`

### Symlink Conflict

Home-manager found an existing file. Options:

1. Delete/backup the existing file, rebuild
2. Set `home.file."path".force = true;`
3. The `backupFileExtension = "backup"` setting auto-renames conflicts

### Neovim Plugins Not Installing

First launch triggers plugin install:

```bash
nvim  # Wait for Lazy to install plugins
:Lazy sync  # Force sync if needed
```

## Useful Commands Reference

| Command                                     | Description                    |
| ------------------------------------------- | ------------------------------ |
| `rebuild`                                   | Apply configuration changes    |
| `nix flake update`                          | Update all inputs              |
| `nix search nixpkgs <pkg>`                  | Search for packages            |
| `darwin-rebuild --list-generations`         | List system generations        |
| `darwin-rebuild switch --rollback`          | Rollback to previous           |
| `nix-collect-garbage -d`                    | Remove old packages            |
| `nix why-depends /run/current-system <pkg>` | Debug dependencies             |
| `nix-tree`                                  | Visualize package dependencies |

## Resources

- [Nix Manual](https://nixos.org/manual/nix/stable/)
- [nixpkgs Search](https://search.nixos.org/packages)
- [nix-darwin Options](https://daiderd.com/nix-darwin/manual/)
- [Home Manager Options](https://nix-community.github.io/home-manager/options.html)
- [Zero to Nix](https://zero-to-nix.com/) - Beginner guide
- [Nix Pills](https://nixos.org/guides/nix-pills/) - Deep dive

## Quick Reference Card

```bash
# Daily use
rebuild                    # Apply config changes
nix flake update && rebuild # Update everything

# Packages
nix search nixpkgs <name>  # Find packages
# Add to home/default.nix → rebuild

# Troubleshooting
darwin-rebuild switch --rollback  # Undo last change
nix-collect-garbage -d            # Free disk space

# Secrets
vim ~/.secrets             # Edit secrets (never commit!)
source ~/.secrets          # Reload in current shell
```

## Installed Tools Reference

### Modern CLI Replacements

These tools replace/improve standard Unix utilities. All installed via Nix in `home/default.nix`.

| Alias | Tool | Replaces | Description |
|-------|------|----------|-------------|
| `ls` | eza | ls | Icons, git status, colors |
| `cat` | bat | cat | Syntax highlighting, git integration |
| `grep` | ripgrep (rg) | grep | 10x faster, respects .gitignore |
| `find` | fd | find | Simpler syntax, faster |
| `sed` | sd | sed | Intuitive syntax: `sd 'find' 'replace'` |
| `ps` | procs | ps | Colorful, human-readable |
| `du` | dust | du | Visual disk usage tree |
| `df` | duf | df | Disk free with colors |
| `top` | bottom (btm) | htop | Graphs, GPU, battery stats |
| `man` | tealdeer (tldr) | man | Examples instead of manuals |
| `ping` | gping | ping | Graphical ping |
| `dig` | dog | dig | Colorful DNS client |
| `curl` | xh | curl/httpie | Friendly HTTP client |

### Additional CLI Tools

| Tool | Purpose | Usage |
|------|---------|-------|
| **yazi** | Terminal file manager | `yazi` or `ya` - blazing fast, image previews |
| **zellij** | Terminal multiplexer | `zj` - modern tmux replacement |
| **glow** | Markdown renderer | `glow README.md` |
| **fx** | Interactive JSON viewer | `fx data.json` or `curl ... \| fx` |
| **ouch** | Archive tool | `ouch compress/decompress` - handles all formats |
| **viddy** | Modern watch | `viddy 'command'` - diff highlighting |
| **grex** | Regex generator | `grex 'foo' 'bar'` - generates regex from examples |
| **mtr** | Network diagnostics | `mtr google.com` - ping + traceroute |
| **age** | File encryption | `age -e -r <key> file` - simple encryption |
| **broot** | Directory navigator | `br` - interactive tree with fuzzy search |

### Navigation & History

| Tool | Trigger | Description |
|------|---------|-------------|
| **zoxide** | `cd` | Smart cd - learns your habits. `cd project` jumps anywhere |
| **atuin** | `Ctrl+R` | Fuzzy search shell history across machines |
| **fzf** | `Ctrl+T` | Fuzzy file finder. `Alt+C` for directories |

### Git Tools

| Tool | Usage | Description |
|------|-------|-------------|
| **lazygit** | `lg` | TUI git client - visual rebasing, staging |
| **delta** | automatic | Better git diffs with syntax highlighting |
| **difftastic** | `git dft` | Structural diff - understands code syntax |
| **gh** | `gh pr create` | GitHub CLI |

### Dev Tools

| Tool | Usage | Description |
|------|-------|-------------|
| **just** | `j <recipe>` | Task runner (better make) |
| **watchexec** | `watchexec -e rs cargo test` | Run commands on file changes |
| **hyperfine** | `hyperfine 'cmd1' 'cmd2'` | Benchmarking tool |
| **tokei** | `tokei` | Code statistics |
| **direnv** | automatic | Per-directory environment variables |

## GUI Apps (Homebrew Casks)

Installed via Homebrew in `hosts/darwin.nix`.

### Terminals

| App | Description |
|-----|-------------|
| **Ghostty** | GPU-accelerated, native macOS feel. Config: `~/.config/ghostty/config` |
| **Warp** | AI-integrated terminal with "Blocks" |
| **iTerm2** | Classic macOS terminal |

### Productivity - Must Have

| App | Description | First Setup |
|-----|-------------|-------------|
| **Raycast** | Launcher replacing Spotlight | Set `⌘+Space` in System Settings → Keyboard → Shortcuts |
| **AeroSpace** | i3-like tiling window manager | Config: `~/.aerospace.toml` ([starter config](https://github.com/nikitabobko/AeroSpace)) |
| **OrbStack** | Docker Desktop replacement (10x faster) | Just open it - replaces Docker CLI |
| **Obsidian** | Knowledge management / PKM | Create vault in `~/Documents/Notes` |
| **Granola** | AI meeting notes (no bot joins) | Grant microphone access |

### Productivity - Recommended

| App | Description |
|-----|-------------|
| **Notion Calendar** | Calendar management (formerly Cron) |
| **Loom** | Async video messages |
| **Arc** | Modern browser with Spaces |
| **CleanShot** | Screenshots & screen recording |
| **Ice** | Menu bar management (free, hides icons). Cask: `jordanbaird-ice` |

### macOS UI Enhancements

| App | Description | Setup |
|-----|-------------|-------|
| **SketchyBar** | Custom menu bar replacement | Config: `~/.config/sketchybar/sketchybarrc` |
| **Karabiner-Elements** | Keyboard remapping, Hyper Key | Import "Caps Lock to Hyper" from Complex Modifications |
| **LinearMouse** | Fix mouse acceleration | Disable pointer acceleration for 1:1 movement |
| **DockDoor** | Window previews on Dock | Enable "Hover Previews" in settings |
| **Alt-Tab** | Windows-style window switching | Set to `⌥+Tab` in preferences |
| **HazeOver** | Dim inactive windows | Set dimming to ~30% for subtle focus |
| **Stats** | System stats in menu bar | Choose which stats to display |
| **BetterTouchTool** | Trackpad gestures & automation | License required for full features |
| **Übersicht** | Desktop widgets | Widgets at `~/Library/Application Support/Übersicht/widgets` |

### Quick Look Enhancements

| Plugin | Description |
|--------|-------------|
| **syntax-highlight** | Syntax highlighting for code files |
| **qlmarkdown** | Render Markdown files |
| **qlstephen** | Preview plain text without extension |
| **qlvideo** | Video thumbnails and previews |

## Power User CLI Tools

### Container & Kubernetes

| Tool | Command | Description |
|------|---------|-------------|
| **lazydocker** | `lazydocker` | TUI for Docker management |
| **k9s** | `k9s` | TUI for Kubernetes cluster management |
| **dive** | `dive <image>` | Analyze Docker image layers |
| **ctop** | `ctop` | Top-like container metrics |

### Database Tools

| Tool | Command | Description |
|------|---------|-------------|
| **usql** | `usql postgres://...` | Universal SQL client (20+ databases) |
| **pgcli** | `pgcli -h host -U user dbname` | Smart Postgres CLI |
| **litecli** | `litecli mydb.sqlite` | SQLite with autocomplete |

### API & HTTP Testing

| Tool | Command | Description |
|------|---------|-------------|
| **hurl** | `hurl api_tests.hurl` | HTTP testing with assertions |
| **posting** | `posting` | TUI API client (Postman alternative) |

### Security Scanning

| Tool | Command | Description |
|------|---------|-------------|
| **trivy** | `trivy image nginx` | Vulnerability scanner |
| **gitleaks** | `gitleaks detect` | Find secrets in git history |
| **trufflehog** | `trufflehog git file://.` | Deep secret scanning |

### Data Processing

| Tool | Command | Description |
|------|---------|-------------|
| **miller** | `mlr --csv filter '$age > 30' data.csv` | CSV/JSON Swiss army knife |
| **jnv** | `cat data.json \| jnv` | Interactive jq with live preview |
| **dasel** | `dasel -f config.yaml '.database.host'` | Query structured data |
| **gron** | `gron data.json \| grep name` | Make JSON greppable |

### System Monitoring

| Tool | Command | Description |
|------|---------|-------------|
| **zenith** | `zenith` | System monitor with zoomable charts |
| **glances** | `glances` | Cross-platform monitoring |
| **sampler** | `sampler -c dashboard.yml` | Custom CLI dashboards |

### Demo & Documentation

| Tool | Command | Description |
|------|---------|-------------|
| **vhs** | `vhs record` | Create terminal GIFs as code |
| **gum** | `gum choose "opt1" "opt2"` | Beautiful shell script prompts |
| **slides** | `slides presentation.md` | Terminal presentations |

### Shell & Prompt

| Tool | Description |
|------|-------------|
| **starship** | Cross-shell prompt. Config: `~/.config/starship.toml` |
| **nushell** | Modern shell with structured data. Run: `nu` |
| **fastfetch** | Fast system info display |

## Post-Install Setup

### Raycast (Replace Spotlight)

1. Open Raycast
2. Go to **System Settings → Keyboard → Keyboard Shortcuts → Spotlight**
3. Disable `⌘+Space` for Spotlight
4. In Raycast settings, set hotkey to `⌘+Space`

Recommended extensions:
- GitHub
- Linear (if using)
- Clipboard History
- Window Management

### AeroSpace (Tiling Window Manager)

Create `~/.aerospace.toml`:

```toml
# Start AeroSpace at login
start-at-login = true

# Gaps between windows
[gaps]
inner.horizontal = 10
inner.vertical = 10
outer.left = 10
outer.right = 10
outer.top = 10
outer.bottom = 10

# Key bindings (alt = option key)
[mode.main.binding]
alt-h = 'focus left'
alt-j = 'focus down'
alt-k = 'focus up'
alt-l = 'focus right'

alt-shift-h = 'move left'
alt-shift-j = 'move down'
alt-shift-k = 'move up'
alt-shift-l = 'move right'

alt-1 = 'workspace 1'
alt-2 = 'workspace 2'
alt-3 = 'workspace 3'
alt-4 = 'workspace 4'

alt-shift-1 = 'move-node-to-workspace 1'
alt-shift-2 = 'move-node-to-workspace 2'
alt-shift-3 = 'move-node-to-workspace 3'
alt-shift-4 = 'move-node-to-workspace 4'

alt-f = 'fullscreen'
alt-shift-space = 'layout floating tiling'
alt-slash = 'layout tiles horizontal vertical'
```

### Ghostty

Create `~/.config/ghostty/config`:

```
# Font
font-family = "JetBrainsMono Nerd Font"
font-size = 14

# Theme (or use: theme = catppuccin-mocha)
background = 282a36
foreground = f8f8f2

# Window
window-padding-x = 10
window-padding-y = 10
window-decoration = false

# Shell integration
shell-integration = zsh
```

### Yazi (Terminal File Manager)

Add shell wrapper to get `cd` on exit. Add to `home/default.nix` initContent:

```bash
# Yazi shell wrapper (cd to directory on exit)
function ya() {
  local tmp="$(mktemp -t "yazi-cwd.XXXXX")"
  yazi "$@" --cwd-file="$tmp"
  if cwd="$(cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
    cd -- "$cwd"
  fi
  rm -f -- "$tmp"
}
```

Key bindings in yazi:
- `h/j/k/l` - Navigate
- `Enter` - Open file
- `q` - Quit
- `Space` - Select
- `d` - Delete
- `y` - Copy
- `p` - Paste
- `/` - Search
- `z` - Jump (zoxide integration)

### OrbStack

Just open OrbStack - it automatically:
- Replaces Docker CLI (`docker`, `docker-compose`)
- Provides faster container startup
- Uses less RAM than Docker Desktop
- Integrates with macOS (files, network)

### SketchyBar (Custom Menu Bar)

1. Hide the default menu bar:
   - System Settings → Control Center → Automatically hide and show the menu bar → Always

2. Create config directory:
```bash
mkdir -p ~/.config/sketchybar/plugins
cp $(brew --prefix)/share/sketchybar/examples/sketchybarrc ~/.config/sketchybar/sketchybarrc
cp -r $(brew --prefix)/share/sketchybar/examples/plugins/ ~/.config/sketchybar/plugins/
chmod +x ~/.config/sketchybar/plugins/*
```

3. Start SketchyBar:
```bash
brew services start sketchybar
```

For advanced configs, check [FelixKratz's dotfiles](https://github.com/FelixKratz/dotfiles).

### Karabiner-Elements (Hyper Key Setup)

The **Hyper Key** maps Caps Lock to Cmd+Opt+Ctrl+Shift, giving you a whole new modifier key.

1. Open Karabiner-Elements
2. Go to **Complex Modifications** → **Add rule** → **Import more rules from the Internet**
3. Search for "Caps Lock to Hyper Key" and import it
4. Enable the rule

Now you can set global shortcuts like `Hyper+T` for terminal, `Hyper+B` for browser, etc. in Raycast or other apps.

### Starship Prompt

Create `~/.config/starship.toml`:

```toml
# Minimal, fast prompt
format = """
$directory$git_branch$git_status$character"""

[character]
success_symbol = "[❯](bold green)"
error_symbol = "[❯](bold red)"

[directory]
truncation_length = 3
style = "bold cyan"

[git_branch]
symbol = " "
style = "bold purple"

[git_status]
style = "bold yellow"
```

Add to your shell (already works with zsh via Nix, but for manual setup):
```bash
eval "$(starship init zsh)"
```

### Lazydocker

Just run `lazydocker` to get a full TUI for Docker management.

Key bindings:
- `j/k` - Navigate
- `Enter` - Select
- `d` - Delete
- `r` - Restart container
- `s` - Stop container
- `a` - Attach to container
- `L` - View logs
- `[/]` - Switch panels

### K9s (Kubernetes TUI)

Run `k9s` in any directory with kubectl configured.

Key bindings:
- `:` - Command mode (type resource names like `pods`, `deploy`, `svc`)
- `/` - Filter
- `l` - Logs
- `s` - Shell into pod
- `d` - Describe
- `ctrl-k` - Kill pod
- `?` - Help

### Hurl (API Testing)

Create a `.hurl` file:

```hurl
# Get request
GET https://api.example.com/users

HTTP 200
[Asserts]
jsonpath "$.users" count > 0

# Chained POST request
POST https://api.example.com/users
Content-Type: application/json
{
    "name": "test"
}

HTTP 201
[Captures]
user_id: jsonpath "$.id"
```

Run with:
```bash
hurl --test api.hurl
```

### Alt-Tab Setup

1. Open Alt-Tab preferences
2. Set trigger to `⌥ + Tab`
3. Enable "Show windows from all Spaces"
4. Set appearance to "Windows 10" style

### Nerd Fonts

The config installs several Nerd Fonts. To use in terminals:
- **Ghostty**: Set `font-family = "JetBrainsMono Nerd Font"`
- **iTerm2**: Profiles → Text → Font → Select a Nerd Font
- **Warp**: Settings → Appearance → Font Family
