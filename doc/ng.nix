# Architecture of the cleaned-up Haskell packages set in Nixpkgs.

let

  pkgs = import <nixpkgs> {};
  lib = pkgs.lib;

  inherit (pkgs) newScope stdenv fetchurl;
  ghc = pkgs.haskellPackages.ghcPlain;

  fix = f: let x = f x // { __unfix__ = f; }; in x;

  extend = rattrs: f: self: let super = rattrs self; in super // f self super;

  buildCabal = import ./generic-builder.nix { inherit stdenv ghc fetchurl; };

  hpkgs = self:
    let

      definePackage = args: pkg: newScope self pkg args;

    in
      import ../hackage-packages.nix { inherit stdenv ghc pkgs definePackage; } // {

        inherit buildCabal;

        mtl21 = definePackage {}
                ({ buildCabal, transformers }:
                 buildCabal {
                   pname = "mtl";
                   version = "2.1.3.1";
                   sha256 = "1xpn2wjmqbh2cg1yssc6749xpgcqlrrg4iilwqgkcjgvaxlpdbvp";
                   buildDepends = [ transformers ];
                   meta = {
                     homepage = "http://github.com/ekmett/mtl";
                     description = "Monad classes, using functional dependencies";
                     license = stdenv.lib.licenses.bsd3;
                     platforms = ghc.meta.platforms;
                   };
                  });

      };

  defaultConfiguration = self: super: {
    mtl22 = super.mtl.override { transformers = super.transformers; };
    mtl = self.mtl21.override { transformers = null; };
    zlib = super.zlib.override { zlib = pkgs.zlib; };
    cabal2nix = super.cabal2nix.deepOverride { mtl = self.mtl22; };

    Cabal = null;
    array = null;
    base = null;
    binPackageDb = null;
    binary = null;
    bytestring = null;
    containers = null;
    deepseq = null;
    directory = null;
    filepath = null;
    ghc = null;
    ghcPrim = null;
    haskeline = null;
    haskell2010 = null;
    haskell98 = null;
    hoopl = null;
    hpc = null;
    integerGmp = null;
    oldLocale = null;
    oldTime = null;
    pretty = null;
    process = null;
    rts = null;
    templateHaskell = null;
    terminfo = null;
    time = null;
    transformers = null;
    unix = null;
    xhtml = null;
  };

  haskellPackages = fix (extend hpkgs defaultConfiguration);
in

  haskellPackages # .hsemail.deepOverride { mtl = haskellPackages.mtl2; }
