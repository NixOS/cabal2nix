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
      haskellPackages = pkgs.lib.getAttrFromPath ["haskell" "packages" ghcVer] pkgs;
    in
    haskellPackages.mkDerivation {
      pname = "cabal2nix";
      version = cabal2nixSrc.gitTag;
      src = cabal2nixSrc;
      isLibrary = true;
      isExecutable = true;
      libraryHaskellDepends = with haskellPackages; [
        aeson ansi-wl-pprint base bytestring Cabal containers data-default
        deepseq deepseq-generics directory doctest filepath hackage-db
        hspec lens monad-par monad-par-extras mtl optparse-applicative
        pretty process regex-posix SHA split transformers utf8-string
      ];
      executableHaskellDepends = with haskellPackages; [
        aeson ansi-wl-pprint base bytestring Cabal containers data-default
        deepseq deepseq-generics directory doctest filepath hackage-db
        hspec lens monad-par monad-par-extras mtl optparse-applicative
        pretty process regex-posix SHA split transformers utf8-string
      ];
      testHaskellDepends = with haskellPackages; [
        aeson ansi-wl-pprint base bytestring Cabal containers data-default
        deepseq deepseq-generics directory doctest filepath hackage-db
        hspec lens monad-par monad-par-extras mtl optparse-applicative
        pretty process regex-posix SHA split transformers utf8-string
      ];
      homepage = "https://github.com/nixos/cabal2nix#readme";
      description = "Convert Cabal files into Nix build instructions";
      license = pkgs.stdenv.lib.licenses.bsd3;
      maintainers = [ pkgs.stdenv.lib.maintainers.simons ];
    }
  ));
}
