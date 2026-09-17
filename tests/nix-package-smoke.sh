#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

# Tek9 must be consumable as a flake package, not only through a developer
# checkout/devShell.
nix build --no-link .#tek9
nix build --no-link .#default

system="$(nix eval --raw --impure --expr builtins.currentSystem)"

# The Tek9 check wraps the package in a downstream SBCL, changes to an empty
# consumer directory, loads :tek9 from the package closure, and opens a mutable
# LMDB database under the build directory rather than the immutable Nix store.
nix build --no-link ".#checks.${system}.package-smoke"

# star-git is a separate downstream-consumable ASDF package. Its smoke check
# intentionally unsets LD_LIBRARY_PATH before SBCL starts, then exercises the
# real blob -> commit -> LZMA pack -> fresh repository import path. This proves
# liblzma arrives through the Nix package closure rather than an ambient host
# library or the development shell.
nix build --no-link .#star-git
nix build --no-link ".#checks.${system}.star-git-package-smoke"
