{ pkgs ? import <nixpkgs> { }
, ghcVersion ? pkgs.haskellPackages.ghc.version
, withHls ? true
}:

let
  haskellPackages = pkgs.haskell.packages."ghc${
    builtins.replaceStrings [ "." ] [ "" ] ghcVersion
  }";
  ghc = haskellPackages.ghcWithHoogle (hps: [
    hps.ansi-wl-pprint
    hps.hopenssl
    hps.hpack
    hps.lens
    hps.optparse-applicative
    hps.pretty
    hps.split
    hps.yaml
    hps.monad-par
    hps.monad-par-extras
    hps.tasty
    hps.tasty-golden
    hps.utf8-string
    hps.tar
    hps.hspec
    hps.parsec-class
  ]);

in pkgs.mkShell {
  packages = [
    ghc
    pkgs.cabal-install
    pkgs.haskell-ci
    (pkgs.lib.getLib pkgs.openssl)
  ] ++ pkgs.lib.optionals withHls [
    haskellPackages.haskell-language-server
  ];
}
