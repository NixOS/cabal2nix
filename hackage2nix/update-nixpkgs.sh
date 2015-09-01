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
git pull -q
rm -f preferred-versions
for n in */preferred-versions; do
  cat >>preferred-versions "$n"
  echo >>preferred-versions
done
DIR=$HOME/.cabal/packages/hackage.haskell.org
TAR=$DIR/00-index.tar
TARGZ=$TAR.gz
mkdir -p "$DIR"
rm -f "$TAR" "$TARGZ"
git archive --format=tar -o "$TAR" HEAD
gzip -k "$TAR"
hackage=$(git rev-parse --verify HEAD)
cd ..

stack --verbosity 0 build hackage2nix
.stack-work/dist/x86_64-linux/Cabal-1.22.4.0/build/hackage2nix/hackage2nix >hackage-packages.nix

cd nixpkgs
git pull -q
mv ../hackage-packages.nix pkgs/development/haskell-modules/hackage-packages.nix
if [ -n "$(git status --porcelain)" ]; then
  git commit -q -m "hackage-packages.nix: update to https://github.com/commercialhaskell/all-cabal-hashes/commit/$hackage with hackage2nix $cabal2nix" -- pkgs/development/haskell-modules/hackage-packages.nix
  git push -q
fi
