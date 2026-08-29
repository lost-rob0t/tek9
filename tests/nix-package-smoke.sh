#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

# RED contract for #7: Tek9 must be consumable as a flake package, not only
# through a developer checkout/devShell.
nix build --no-link .#tek9
nix build --no-link .#default

# The flake check wraps the package in a downstream SBCL, changes to an empty
# consumer directory, loads :tek9 from the package closure, and opens a mutable
# LMDB database under the build directory rather than the immutable Nix store.
nix build --no-link .#checks."$(nix eval --raw --impure --expr builtins.currentSystem)".package-smoke
