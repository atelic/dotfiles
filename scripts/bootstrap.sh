#!/usr/bin/env bash
set -euo pipefail

echo "=== Post-Rebuild Bootstrap ==="
echo "Run this after 'darwin-rebuild switch' on a new machine"
echo ""

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

info() { echo -e "${GREEN}[INFO]${NC} $1"; }
warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
prompt() { echo -e "${YELLOW}[ACTION]${NC} $1"; }

check_done() {
	if [[ "$1" == "y" || "$1" == "Y" ]]; then
		echo -e "${GREEN}Done${NC}"
	else
		echo -e "${YELLOW}Skipped${NC}"
	fi
}

echo "=== 1. Secrets File ==="
if [[ -f ~/.secrets ]]; then
	info "~/.secrets already exists"
else
	warn "~/.secrets not found"
	prompt "Create it now? (y/n)"
	read -r create_secrets
	if [[ "$create_secrets" == "y" ]]; then
		cat >~/.secrets <<'EOF'
# API Keys and Secrets
# This file is sourced by your shell - never commit it!

# AI
export ANTHROPIC_API_KEY=""
export OPENAI_API_KEY=""

# GitHub (for tools that need it beyond gh auth)
export GITHUB_TOKEN=""

# Add more secrets as needed
EOF
		chmod 600 ~/.secrets
		info "Created ~/.secrets - edit it to add your keys"
		echo "  nvim ~/.secrets"
	fi
fi

echo ""
echo "=== 2. GitHub CLI Authentication ==="
if gh auth status &>/dev/null; then
	info "GitHub CLI already authenticated"
else
	warn "GitHub CLI not authenticated"
	prompt "Run 'gh auth login' now? (y/n)"
	read -r do_gh_auth
	if [[ "$do_gh_auth" == "y" ]]; then
		gh auth login
	fi
fi

echo ""
echo "=== 3. SSH Keys ==="
if [[ -f ~/.ssh/id_ed25519 ]] || [[ -f ~/.ssh/id_rsa ]]; then
	info "SSH keys found"
else
	warn "No SSH keys found"
	prompt "Generate new SSH key? (y/n)"
	read -r gen_ssh
	if [[ "$gen_ssh" == "y" ]]; then
		echo "Enter email for SSH key:"
		read -r ssh_email
		ssh-keygen -t ed25519 -C "$ssh_email"
		eval "$(ssh-agent -s)"
		ssh-add ~/.ssh/id_ed25519
		echo ""
		info "Add this public key to GitHub:"
		echo "  https://github.com/settings/ssh/new"
		echo ""
		cat ~/.ssh/id_ed25519.pub
		echo ""
		prompt "Press enter when done..."
		read -r
	fi
fi

echo ""
echo "=== 4. Neovim Plugins ==="
info "LazyVim will auto-install plugins on first launch"
prompt "Open Neovim now to trigger install? (y/n)"
read -r do_nvim
if [[ "$do_nvim" == "y" ]]; then
	nvim +q
	info "Plugins installed"
fi

echo ""
echo "=== 5. GitHub Copilot Authentication ==="
prompt "Authenticate Copilot in Neovim? (y/n)"
read -r do_copilot
if [[ "$do_copilot" == "y" ]]; then
	info "Running :Copilot auth in Neovim..."
	nvim -c "Copilot auth" -c "sleep 5" -c "q"
fi

echo ""
echo "=== 6. Atuin Shell History Sync (Optional) ==="
if command -v atuin &>/dev/null; then
	prompt "Set up Atuin cloud sync? (y/n)"
	read -r do_atuin
	if [[ "$do_atuin" == "y" ]]; then
		atuin login
		atuin sync
	fi
else
	warn "Atuin not found"
fi

echo ""
echo "=== 7. macOS Permissions ==="
info "Some apps need manual permission grants:"
echo "  - System Settings > Privacy & Security > Accessibility"
echo "    Grant: AeroSpace, Raycast, Karabiner-Elements"
echo "  - System Settings > Privacy & Security > Input Monitoring"
echo "    Grant: Karabiner-Elements"
echo ""
prompt "Press enter when permissions are configured..."
read -r

echo ""
echo "=== 8. Raycast Setup ==="
info "To replace Spotlight with Raycast:"
echo "  1. System Settings > Keyboard > Keyboard Shortcuts > Spotlight"
echo "  2. Disable 'Show Spotlight search' (Cmd+Space)"
echo "  3. Open Raycast > Settings > Set hotkey to Cmd+Space"
echo ""
prompt "Press enter when done..."
read -r

echo ""
echo "=== 9. Final Steps ==="
info "Recommended next steps:"
echo "  1. Restart your terminal to pick up all changes"
echo "  2. Run 'rebuild' to verify everything works"
echo "  3. Open AeroSpace and grant accessibility permissions"
echo "  4. Configure Karabiner-Elements for Hyper key (if desired)"
echo ""

echo -e "${GREEN}=== Bootstrap Complete ===${NC}"
