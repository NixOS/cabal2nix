#! /usr/bin/env bash

set -eu -o pipefail

# List package sets with:
#   nix-instantiate --eval -E 'with (import <nixpkgs> {}); builtins.attrNames haskell.packages'
pkgsets=( "ghc7102" "lts-0_0" "lts-0_1" "lts-0_2" "lts-0_3" "lts-0_4" "lts-0_5"
          "lts-0_6" "lts-0_7" "lts-1_0" "lts-1_1" "lts-1_10" "lts-1_11"
          "lts-1_12" "lts-1_13" "lts-1_14" "lts-1_15" "lts-1_2" "lts-1_4"
          "lts-1_5" "lts-1_7" "lts-1_8" "lts-1_9" "lts-2_0" "lts-2_1"
          "lts-2_10" "lts-2_11" "lts-2_12" "lts-2_13" "lts-2_14" "lts-2_15"
          "lts-2_16" "lts-2_17" "lts-2_18" "lts-2_19" "lts-2_2" "lts-2_20"
          "lts-2_21" "lts-2_22" "lts-2_3" "lts-2_4" "lts-2_5" "lts-2_6"
          "lts-2_7" "lts-2_8" "lts-2_9" "lts-3_0" "lts-3_1" "lts-3_2" "lts-3_3"
          "lts-3_4" "lts-3_5" "lts-3_6" "lts-3_7" "lts-3_8" "lts-3_9"
        )

for n in "${pkgsets[@]}"; do
  nix-instantiate --eval --strict --json list-haskell-store-paths.nix --argstr pkgset $n >$n.json
done
