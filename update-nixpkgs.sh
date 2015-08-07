#! /usr/bin/env bash

set -eu -o pipefail

exit_trap()
{
  local lc="$BASH_COMMAND" rc=$?
  test $rc -eq 0 || echo "*** error $rc: $lc"
}

trap exit_trap EXIT

cd "$(dirname "$0")"
cabal2nix=$(git describe --dirty)

cd nixpkgs
git reset -q --hard
git pull -q
export NIX_PATH=nixpkgs=$PWD
cd ..

cd hackage
# git reset -q --hard
git pull -q
DIR=$HOME/.cabal/packages/hackage.haskell.org
TAR=$DIR/00-index.tar
TARGZ=$TAR.gz
mkdir -p "$DIR"
rm -f "$TAR" "$TARGZ"
git archive --format=tar -o "$TAR" HEAD
gzip -k "$TAR"
hackage=$(git rev-parse --verify HEAD)
cd ..

rm -rf dist
make --quiet cabal2nix.cabal
cabal build -v0 -j hackage2nix
dist/build/hackage2nix/hackage2nix >hackage-packages.nix 2>/dev/null

cd nixpkgs
git pull -q
mv ../hackage-packages.nix pkgs/development/haskell-modules/hackage-packages.nix
if [ -n "$(git status --porcelain)" ]; then
  git commit -q -m "hackage-packages.nix: update to https://github.com/commercialhaskell/all-cabal-files/commit/$hackage with hackage2nix revision $cabal2nix" -- pkgs/development/haskell-modules/hackage-packages.nix
  git push -q
fi
