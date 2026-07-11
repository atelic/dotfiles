#!/usr/bin/env bash
set -Eeuo pipefail

usage() {
  cat <<'EOF'
Usage: scripts/update-everything.sh [options]

Updates the laptop's Nix flake inputs and nix-darwin/Home Manager system,
Homebrew formulae and casks, and available macOS system software.

Options:
  --skip-nix       Skip Nix and nix-darwin/Home Manager updates
  --skip-brew      Skip the explicit Homebrew update pass
  --skip-macos     Skip macOS Software Update
  --gc             Garbage-collect unused Nix store paths after updating
  -h, --help       Show this help

The script never restarts the Mac automatically. If macOS needs a restart,
Software Update will report it when the update finishes.
EOF
}

log() {
  printf '\n[update-all] %s\n' "$*"
}

fail() {
  printf '\n[update-all] error: %s\n' "$*" >&2
  exit 1
}

require_cmd() {
  command -v "$1" >/dev/null 2>&1 || fail "required command not found: $1"
}

run_nix=1
run_brew=1
run_macos=1
run_gc=0

while (($#)); do
  case "$1" in
    --skip-nix) run_nix=0 ;;
    --skip-brew) run_brew=0 ;;
    --skip-macos) run_macos=0 ;;
    --gc) run_gc=1 ;;
    -h | --help)
      usage
      exit 0
      ;;
    *) fail "unknown option: $1" ;;
  esac
  shift
done

repo_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)"

require_cmd sudo

log "refreshing sudo credentials"
sudo -v

# Keep the sudo ticket alive during long Nix builds and macOS downloads.
while true; do
  sudo -n true
  sleep 60
done 2>/dev/null &
sudo_keepalive_pid=$!

cleanup() {
  kill "$sudo_keepalive_pid" 2>/dev/null || true
}
trap cleanup EXIT
trap 'fail "update failed on line $LINENO"' ERR

if ((run_nix)); then
  require_cmd nix
  require_cmd darwin-rebuild

  nix_args=(--skip-profile)
  ((run_gc)) && nix_args+=(--gc)

  log "updating Nix, nix-darwin, and Home Manager"
  # The switch also updates/upgrades all declarative Homebrew packages because
  # hosts/darwin.nix enables homebrew.onActivation autoUpdate and upgrade.
  "$repo_dir/scripts/update-nix-packages.sh" "${nix_args[@]}"

  if profile_entries="$(nix profile list 2>/dev/null)" &&
    [[ -n "${profile_entries//[[:space:]]/}" ]]; then
    log "upgrading imperative Nix profile packages"
    nix profile upgrade --all
  fi
fi

if ((run_brew)); then
  require_cmd brew

  log "updating Homebrew metadata"
  brew update

  log "upgrading Homebrew formulae and casks"
  brew upgrade
  brew upgrade --cask

  log "removing stale Homebrew downloads and old versions"
  brew cleanup
fi

if ((run_macos)); then
  require_cmd softwareupdate

  log "installing available macOS system software (no automatic restart)"
  sudo softwareupdate --install --all --verbose
fi

log "all requested updates completed"
