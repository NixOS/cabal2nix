#! /usr/bin/env bash

set -eu -o pipefail

exit_trap()
{
  local lc="$BASH_COMMAND" rc=$?
  test $rc -eq 0 || echo "*** error $rc: $lc"
}

trap exit_trap EXIT

cd "$(dirname "$0")/nixpkgs"

git reset -q --hard
git clean -dxf -q
git pull -q

./maintainers/scripts/update-cabal2nix-latest.sh

./maintainers/scripts/update-hackage.sh

./maintainers/scripts/regenerate-hackage-packages.sh

git add pkgs/development/haskell-modules
git add pkgs/data/misc/hackage
if [ -n "$(git status --porcelain)" ]; then
  cat <<EOF | git commit -n -q -F -
hackage-packages.nix: automatic Haskell package set update
EOF
  git push -q
fi
