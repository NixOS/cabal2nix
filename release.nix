/* Build instructions for the continuous integration system Hydra. */

{ cabal2nixSrc ? { outPath = ./.; revCount = 0; gitTag = "dirty"; }
, releaseBuild ? false
, supportedPlatforms ? ["x86_64-linux"] ++ (if releaseBuild then ["i686-linux" /*"x86_64-darwin"*/] else [])
, supportedCompilers ? [/*"ghc704" "ghc722" "ghc742" "ghc763"*/ "ghc784" /*"ghcHEAD"*/]
}:

let
  genAttrs = (import <nixpkgs> { }).lib.genAttrs;
in
{
  cabal2nix = genAttrs supportedCompilers (ghcVer: genAttrs supportedPlatforms (system:
    let
      pkgs = import <nixpkgs> { inherit system; };
      haskellPackages = pkgs.lib.getAttrFromPath ["haskell-ng" "packages" ghcVer] pkgs;
      nativeCabal = pkgs.stdenv.lib.versionOlder "7.8" haskellPackages.ghc.version;
      Cabal = if nativeCabal then null else haskellPackages.Cabal_1_20_0_3;
      hackageDb = haskellPackages.hackageDb.override { Cabal = Cabal; };
    in
    haskellPackages.mkDerivation {
      pname = "cabal2nix";
      version = cabal2nixSrc.gitTag;
      src = cabal2nixSrc;
      isLibrary = false;
      isExecutable = true;
      buildDepends = with haskellPackages; [
        aeson base bytestring Cabal containers deepseq deepseq-generics
        directory filepath hackage-db monad-par monad-par-extras mtl pretty
        prettyclass process QuickCheck regex-posix SHA split transformers
        utf8-string
      ];
      testDepends = with haskellPackages; [
        aeson base bytestring Cabal containers deepseq deepseq-generics
        directory doctest filepath hackage-db hspec monad-par
        monad-par-extras mtl pretty prettyclass process QuickCheck
        regex-posix SHA split transformers utf8-string
      ];
      homepage = "http://github.com/NixOS/cabal2nix";
      description = "Convert Cabal files into Nix build instructions";
      license = pkgs.stdenv.lib.licenses.bsd3;
      maintainers = [ pkgs.stdenv.lib.maintainers.simons ];
    }
  ));
}
