{ pkgs ? import <nixpkgs> {} }:

let
  haskellPackages = pkgs.haskellPackages;
  ghc = haskellPackages.ghcWithHoogle (hps: [
    hps.ansi-wl-pprint
    hps.distribution-nixpkgs
    hps.hackage-db
    hps.hopenssl
    hps.hpack
    hps.language-nix
    hps.lens
    hps.optparse-applicative
    hps.pretty
    hps.split
    hps.yaml
    hps.monad-par
    hps.monad-par-extras
    hps.tasty
    hps.tasty-golden
  ]);

in pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = [
            ghc
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            (pkgs.lib.getLib pkgs.openssl)
          ];
}
