/* Build instructions for the continuous integration system Hydra. */

{ cabal2nixSrc ? { outPath = ./.; revCount = 0; gitTag = "dirty"; }
, releaseBuild ? false
, supportedPlatforms ? if releaseBuild then ["i686-linux" "x86_64-linux" /*"x86_64-darwin"*/] else ["x86_64-linux"]
, supportedCompilers ? if true then ["ghc784"] else ["ghc704" "ghc722" "ghc742" "ghc763" "ghc784" /*"ghcHEAD"*/]
}:

let
  genAttrs = (import <nixpkgs> { }).lib.genAttrs;
in
{
  cabal2nix = genAttrs supportedCompilers (ghcVer: genAttrs supportedPlatforms (system:
    let
      pkgs = import <nixpkgs> { inherit system; };
      haskellPackages = pkgs.lib.getAttrFromPath ["haskellPackages_${ghcVer}"] pkgs;
      nativeCabal = pkgs.stdenv.lib.versionOlder "7.8" haskellPackages.ghc.version;
      Cabal = if nativeCabal then null else haskellPackages.Cabal_1_18_1_3;
      hackageDb = haskellPackages.hackageDb.override { Cabal = Cabal; };
    in
    haskellPackages.cabal.mkDerivation (self: {
      pname = "cabal2nix";
      src = cabal2nixSrc;
      version = cabal2nixSrc.gitTag;
      isLibrary = false;
      isExecutable = true;
      buildDepends = with haskellPackages; [
        Cabal deepseq filepath hackageDb monadPar monadParExtras mtl
        regexPosix transformers SHA
      ];
      testDepends = with haskellPackages; [ doctest ];
      doCheck = nativeCabal;
      meta = {
        homepage = "http://github.com/NixOS/cabal2nix";
        description = "Convert Cabal files into Nix build instructions";
        license = self.stdenv.lib.licenses.bsd3;
        maintainers = [ self.stdenv.lib.maintainers.simons ];
      };
    })));
}
