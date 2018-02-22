#! /usr/bin/env bash

set -eu -o pipefail

rev=$( git ls-remote --heads git://github.com/commercialhaskell/all-cabal-hashes.git hackage \
     | cut -f1
     )
url="https://github.com/commercialhaskell/all-cabal-hashes/archive/$rev.tar.gz"
sha=$( nix-prefetch-url "$url" )

sed -e "s|url = .*;|url = \"$url\";|"           \
    -e "s|sha256 = .*;|sha256 = \"$sha\";|"     \
    -i nixpkgs/pkgs/data/misc/hackage/default.nix

cat <<EOF
Verify that the new snapshot actually works:

nix-build --no-out-link -E 'with (import ./nixpkgs {}); haskellPackages.callHackage "hsdns" "1.7.1" {}'
EOF
