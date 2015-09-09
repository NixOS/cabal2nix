#! /usr/bin/env bash

set -eu -o pipefail

exit_trap()
{
  local lc="$BASH_COMMAND" rc=$?
  test $rc -eq 0 || echo "*** error $rc: $lc"
}

trap exit_trap EXIT

cd "$(dirname "$0")"

{
  echo "# pkgs/development/haskell-modules/configuration-stackage-lts.yaml"
  echo ""
  echo "extra-packages:"
  curl --silent -L https://www.stackage.org/lts/cabal.config |
      sed -r -e '/installed,$/d' -e '/^--/d' -e 's/constraints: +/ /' -e 's/^ +(.*)$/ - \1/' -e 's/,$//'
} >configuration-stackage-lts.yaml

cd nixpkgs
git pull -q
mv ../configuration-stackage-lts.yaml pkgs/development/haskell-modules/
if [ -n "$(git status --porcelain)" ]; then
  git commit -q -m "configuration-stackage-lts.yaml: update from https://www.stackage.org/lts/cabal.config" -- pkgs/development/haskell-modules/configuration-stackage-lts.yaml
  git push -q
fi
