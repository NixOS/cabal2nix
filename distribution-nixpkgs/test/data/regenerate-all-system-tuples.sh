#!/usr/bin/env bash
#
# Usage: ./regenerate-all-system-tuples.sh [PATH TO NIXPKGS]

curdir="$(dirname "$0")"
expr="$curdir/all-system-tuples.nix"
out="$curdir/all-system-tuples.json"

nixpkgs="${1:-<nixpkgs>}"

nix-instantiate \
    --eval --strict --json \
    "$expr" \
    --arg nixpkgsSrc "$nixpkgs" \
    > "$out"
