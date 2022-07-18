{ nixpkgsSrc ? <nixpkgs> }:

let
  pkgs = import nixpkgsSrc {};
  inherit (pkgs) lib;
  sortStrings = lib.sort (x: y: x < y);

in

lib.pipe (
  lib.mapAttrsToList (
    _: v: (builtins.tryEval (lib.systems.elaborate v).system).value
  ) lib.systems.examples
  ++ lib.platforms.all
) [
  (builtins.filter builtins.isString)
  sortStrings
  lib.unique
]
