#! /usr/bin/env bash

set -eu -o pipefail

exit_trap()
{
  local lc="$BASH_COMMAND" rc=$?
  test $rc -eq 0 || echo "*** error $rc: $lc"
}

trap exit_trap EXIT

cd "$(dirname "$0")/nixpkgs"

./maintainers/scripts/regenerate-hackage-packages.sh "cabal new-run hackage2nix --"
