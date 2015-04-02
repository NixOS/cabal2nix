/* Build instructions for the continuous integration system Hydra. */

{ cabal2nixSrc ? { outPath = ./.; revCount = 0; gitTag = "dirty"; }
, releaseBuild ? false
, supportedPlatforms ? ["x86_64-linux"] ++ (if releaseBuild then ["i686-linux" "x86_64-darwin"] else [])
, supportedCompilers ? ["ghc784" "ghc7101" "ghcHEAD"]
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
      hackage-db = haskellPackages.hackage-db.override { Cabal = Cabal; };
      nativePrettyClass = pkgs.stdenv.lib.versionOlder "7.10" haskellPackages.ghc.version;
      prettyclass = if nativePrettyClass then null else haskellPackages.prettyclass;
    in
    haskellPackages.mkDerivation {
      pname = "cabal2nix";
      version = cabal2nixSrc.gitTag;
      src = cabal2nixSrc;
      isLibrary = false;
      isExecutable = true;
      preConfigure = "runhaskell $setupCompileFlags generate-cabal-file.hs >cabal2nix.cabal";
      buildTools = with haskellPackages; [ cartel pkgs.git ];
      buildDepends = with haskellPackages; [
        aeson base bytestring Cabal containers deepseq deepseq-generics
        directory filepath hackage-db monad-par monad-par-extras mtl pretty
        prettyclass process QuickCheck regex-posix SHA split transformers
        utf8-string lens
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
