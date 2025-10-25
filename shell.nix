{ pkgs ?
    import (builtins.fetchTarball {
      # nixos-unstable 2025-10-23
      url = "https://github.com/nixos/nixpkgs/archive/01f116e4df6a15f4ccdffb1bcd41096869fb385c.tar.gz";
      sha256 = "sha256-f/QCJM/YhrV/lavyCVz8iU3rlZun6d+dAiC3H+CDle4=";
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
  inherit (pkgs) lib;
  haskellLib = pkgs.haskell.lib.compose;

  haskell-ci-pinned = lib.pipe
    pkgs.haskellPackages.haskell-ci
    [
      (haskellLib.overrideSrc {
        version = "0-unstable-2025-06-04";
        src = pkgs.fetchFromGitHub rec {
          name = "haskell-ci-source-${lib.substring 0 7 rev}";
          owner = "haskell-CI";
          repo = "haskell-ci";
          rev = "bdc3cb2907c905fb35012c84d813c47223880aae";
          sha256 = "sha256-Y1DccXXaRMYqZCV6s+9h2HfnNz69WGzH1QPLojUOqII=";
        };
      })
      # Make the build a bit less expensive
      haskellLib.dontCheck
      haskellLib.disableLibraryProfiling
    ];

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
    (lib.getLib pkgs.openssl)
    # Needed to run `cabal2nix`:
    pkgs.nix-prefetch-scripts
  ] ++ lib.optionals withHls [
    haskellPackages.haskell-language-server
  ] ++ lib.optionals (!minimal) [
    haskell-ci-pinned
  ];

  # Make Paths_ module of distribution-nixpkgs find its data files in the shell.
  # https://cabal.readthedocs.io/en/latest/cabal-package.html#accessing-data-files-from-package-code
  distribution_nixpkgs_datadir = toString ./distribution-nixpkgs;
}
