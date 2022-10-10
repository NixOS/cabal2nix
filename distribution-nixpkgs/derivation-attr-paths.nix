{ nixpkgsPath ? <nixpkgs>
, nixpkgsArgs ? {}
}@args:

let
  pkgs = import nixpkgsPath nixpkgsArgs;

  inherit (pkgs) lib;

  /* Condition for us to recurse:
     Either we are at the top-level or
     recurseForDerivations is true.

     Type :: list any -> any -> bool
  */
  recurseInto = path: x: path == [] ||
    (lib.isAttrs x && (x.recurseForDerivations or false));

  /* Takes the nixpkgs set and returns all attribute paths
     to reachable derivations within it as a list of lists of
     strings.

     Type :: attrs -> list (list string)
  */
  derivationPaths =
    let
      go = path: x:
        let
          inherit (builtins.tryEval x)
            value
            success
            ;
        in
          if !success then []
          else if lib.isDerivation value then [
            path
          ] else if recurseInto path x then lib.concatLists (
            lib.mapAttrsToList (n: go (path ++ [ n ])) x
          ) else [];
    in go [];
in
  derivationPaths pkgs
