{ pkgs ?
    import (builtins.fetchTarball {
      # nixos-unstable 2025-04-23
      url = "https://github.com/nixos/nixpkgs/archive/96d30055a2215e5c5a545872376137a5d063e804.tar.gz";
      sha256 = "0xvzkpgc8qy4q252c3x399c8ikrks970c877s4i7vppnhxp08p8n";
    }) { }
, ghcVersion ? pkgs.haskellPackages.ghc.version
  # Pass --arg minimal true to disable tools that are not strictly necessary
  # and may break when using non default GHC versions / other Nixpkgs revisions.
, minimal ? false
, withHls ? !minimal
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
    # Needed to run `cabal2nix`:
    pkgs.nix-prefetch-scripts
  ] ++ pkgs.lib.optionals withHls [
    haskellPackages.haskell-language-server
  ];

  # Make Paths_ module of distribution-nixpkgs find its data files in the shell.
  # https://cabal.readthedocs.io/en/latest/cabal-package.html#accessing-data-files-from-package-code
  distribution_nixpkgs_datadir = toString ./distribution-nixpkgs;
}
