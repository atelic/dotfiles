#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: scripts/update-nix-packages.sh [--skip-profile] [--gc]

Updates all flake inputs, applies the nix-darwin/home-manager system, and
upgrades imperative Nix profile packages when any are present.

Options:
  --skip-profile  Do not run `nix profile upgrade --all`
  --gc            Run `nix store gc` after a successful switch
  -h, --help      Show this help
EOF
}

log() {
  printf '[update-nix] %s\n' "$*"
}

fail() {
  printf '[update-nix] error: %s\n' "$*" >&2
  exit 1
}

require_cmd() {
  command -v "$1" >/dev/null 2>&1 || fail "required command not found: $1"
}

run_profile=1
run_gc=0

while (($#)); do
  case "$1" in
    --skip-profile)
      run_profile=0
      ;;
    --gc)
      run_gc=1
      ;;
    -h | --help)
      usage
      exit 0
      ;;
    *)
      fail "unknown option: $1"
      ;;
  esac
  shift
done

repo_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)"
darwin_configuration="${DARWIN_CONFIGURATION:-Erics-Laptop}"

require_cmd nix
require_cmd sudo
require_cmd darwin-rebuild

cd "$repo_dir"
[[ -f flake.nix ]] || fail "flake.nix not found in $repo_dir"

log "using flake at $repo_dir"
log "refreshing sudo credentials"
sudo -v

while true; do
  sudo -n true
  sleep 60
done 2>/dev/null &
sudo_keepalive_pid=$!

cleanup() {
  kill "$sudo_keepalive_pid" 2>/dev/null || true
}
trap cleanup EXIT

log "updating all flake inputs"
nix flake update

log "applying nix-darwin and home-manager configuration"
# Limit concurrent derivations. Several large Rust packages use LTO and can
# otherwise make Apple's linker crash under memory pressure.
sudo darwin-rebuild switch --max-jobs 1 --cores 8 \
  --flake "$repo_dir#$darwin_configuration"

if ((run_profile)); then
  if profile_entries="$(nix profile list 2>/dev/null)" && [[ -n "${profile_entries//[[:space:]]/}" ]]; then
    log "upgrading imperative Nix profile packages"
    nix profile upgrade --all
  else
    log "no imperative Nix profile packages found; skipping profile upgrade"
  fi
fi

if ((run_gc)); then
  log "garbage collecting old Nix store paths"
  nix store gc
fi

log "recent nix-darwin generations"
sudo darwin-rebuild --list-generations | tail -n 5 || true

log "done"
