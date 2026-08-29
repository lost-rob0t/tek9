#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

# RED contract for #7: Tek9 must be consumable as a flake package, not only
# through a developer checkout/devShell.
nix build --no-link .#tek9
nix build --no-link .#default

# A downstream consumer must be able to load :tek9 with only the package's
# exported ASDF registry, outside the Tek9 source tree.
consumer="$(mktemp -d)"
trap 'rm -rf "$consumer"' EXIT
cd "$consumer"

tek9_pkg="$(nix build --no-link --print-out-paths "$repo_root#tek9")"
CL_SOURCE_REGISTRY="$tek9_pkg//" \
  nix shell nixpkgs#sbcl -c sbcl --non-interactive \
    --eval '(require :asdf)' \
    --eval '(asdf:load-system :tek9)' \
    --eval '(assert (find-package :tek9))'
