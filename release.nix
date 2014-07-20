/* Build instructions for the continuous integration system Hydra. */

{ cabal2nixSrc ? { outPath = ./.; revCount = 0; gitTag = "dirty"; }
, supportedPlatforms ? [ "x86_64-linux" ]
, supportedCompilers ? ["ghc6104" "ghc6123" "ghc704" "ghc722" "ghc742" "ghc763" "ghc783" "ghcHEAD"]
}:

let
  genAttrs = (import <nixpkgs> { }).lib.genAttrs;
in
{
  cabal2nix = genAttrs supportedCompilers (ghcVer: genAttrs supportedPlatforms (system:
    let
      pkgs = import <nixpkgs> { inherit system; };
      haskellPackages = pkgs.lib.getAttrFromPath ["haskellPackages_${ghcVer}"] pkgs;
    in
    haskellPackages.cabal.mkDerivation (self: {
      pname = "cabal2nix";
      src = cabal2nixSrc;
      version = cabal2nixSrc.gitTag;
      isLibrary = false;
      isExecutable = true;
      buildDepends = with haskellPackages; [ Cabal filepath hackageDb mtl regexPosix transformers ];
      testDepends = with haskellPackages; [ doctest ];
      doCheck = self.stdenv.lib.versionOlder "7.6" self.ghc.version;
      meta = {
        homepage = "http://github.com/NixOS/cabal2nix";
        description = "Convert Cabal files into Nix build instructions";
        license = self.stdenv.lib.licenses.bsd3;
        maintainers = [ self.stdenv.lib.maintainers.simons ];
      };
    })));
}
