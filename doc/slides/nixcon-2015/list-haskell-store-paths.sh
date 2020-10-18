#! /usr/bin/env bash

set -eu -o pipefail

pkgsets=( $(nix-instantiate --eval -E 'with (import <nixpkgs> {}); builtins.attrNames haskell.packages') )
n=$(( ${#pkgsets[@]} - 2 ))

for n in ${pkgsets[@]:1:$n}; do
  eval n=$n
  nix-instantiate --eval --strict --json list-haskell-store-paths.nix --argstr pkgset $n >$n.json
done
