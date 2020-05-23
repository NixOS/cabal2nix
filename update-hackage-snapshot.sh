#! /usr/bin/env bash

set -eu -o pipefail

rev=$( git ls-remote --heads git://github.com/commercialhaskell/all-cabal-hashes.git hackage \
     | cut -f1
     )
nix-prefetch-github commercialhaskell all-cabal-hashes --rev "$rev" > nixpkgs/pks/data/misc/hackage/pin.json

cat <<EOF
Verify that the new snapshot actually works:

nix-build --no-out-link -E 'with (import ./nixpkgs {}); haskellPackages.callHackage "hsdns" "1.7.1" {}'
EOF
