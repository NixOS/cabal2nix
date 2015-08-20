/* Build instructions for the continuous integration system Hydra. */

{ cabal2nixSrc ? { outPath = ./.; revCount = 0; gitTag = "dirty"; }
, releaseBuild ? false
, supportedPlatforms ? ["x86_64-linux"] ++ (if releaseBuild then ["i686-linux" "x86_64-darwin"] else [])
, supportedCompilers ? ["ghc7102"]
}:

let
  genAttrs = (import <nixpkgs> { }).lib.genAttrs;
in
{
  cabal2nix = genAttrs supportedCompilers (ghcVer: genAttrs supportedPlatforms (system:
    let
      pkgs = import <nixpkgs> { inherit system; };
      haskellPackages = pkgs.lib.getAttrFromPath ["haskell-ng" "packages" ghcVer] pkgs;
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
        process QuickCheck regex-posix SHA split transformers
        utf8-string lens optparse-applicative data-default
      ];
      testDepends = with haskellPackages; [
        aeson base bytestring Cabal containers deepseq deepseq-generics
        directory doctest filepath hackage-db hspec monad-par
        monad-par-extras mtl pretty process QuickCheck
        regex-posix SHA split transformers utf8-string data-default
      ];
      homepage = "http://github.com/NixOS/cabal2nix";
      description = "Convert Cabal files into Nix build instructions";
      license = pkgs.stdenv.lib.licenses.bsd3;
      maintainers = [ pkgs.stdenv.lib.maintainers.simons ];
    }
  ));
}
