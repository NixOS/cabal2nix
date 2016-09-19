#! /usr/bin/env bash

set -eu -o pipefail

tmpfile=$(mktemp "stackage-update.XXXXXXX")
trap "rm $tmpfile" 0

curl -L -s "https://www.stackage.org/lts/cabal.config" >"$tmpfile"
version=$(sed -n "s/^--.*http:..www.stackage.org.snapshot.lts-//p" "$tmpfile")

sed -r \
    -e '/^--/d' \
    -e 's|^constraints:||' \
    -e 's|^ +|  - |' \
    -e 's|,$||' \
    -e '/installed$/d' \
    -i "$tmpfile"

sed -e '/# LTS Haskell/,/^$/c \TODO\
'   -i nixpkgs/pkgs/development/haskell-modules/configuration-hackage2nix.yaml

sed -e "/TODO/r $tmpfile" \
    -e "s/TODO/  # LTS Haskell $version/" \
    -i nixpkgs/pkgs/development/haskell-modules/configuration-hackage2nix.yaml
