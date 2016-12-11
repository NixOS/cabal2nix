#! /usr/bin/env bash

set -eu -o pipefail

tmpfile=$(mktemp "stackage-update.XXXXXXX")
trap "rm $tmpfile" 0

curl -L -s "https://www.stackage.org/lts/cabal.config" >"$tmpfile"
version=$(sed -n "s/^--.*http:..www.stackage.org.snapshot.lts-//p" "$tmpfile")

# Create a simple yaml version of the file.
sed -r \
    -e '/^--/d' \
    -e 's|^constraints:||' \
    -e 's|^ +|  - |' \
    -e 's|,$||' \
    -e '/installed$/d' \
    -i "$tmpfile"

# Drop restrictions on some tools where we always want the latest version.
sed -r \
    -e '/ cabal-install /d' \
    -e '/ cabal2nix /d' \
    -e '/ hindent /d' \
    -e '/ hoogle /d' \
    -e '/ ShellCheck /d' \
    -e '/ stack /d' \
    -i "$tmpfile"

# Drop the previous configuration ...
sed -e '/# LTS Haskell/,/^$/c \TODO\
'   -i nixpkgs/pkgs/development/haskell-modules/configuration-hackage2nix.yaml

# ... and replace it with the new one.
sed -e "/TODO/r $tmpfile" \
    -e "s/TODO/  # LTS Haskell $version/" \
    -i nixpkgs/pkgs/development/haskell-modules/configuration-hackage2nix.yaml
