/* Build instructions for the continuous integration system Hydra. */

{ cabal2nixSrc ? { outPath = ./.; revCount = 0; gitTag = "dirty"; }
, supportedPlatforms ? [ "x86_64-linux" ]
, supportedCompilers ? ["ghc6123" "ghc704" "ghc722" "ghc742" "ghc763" "ghc782" "ghcHEAD"]
}:

let
  genAttrs = (import <nixpkgs> { }).lib.genAttrs;
in
{
  cabal2nix = genAttrs supportedCompilers (ghcVer: genAttrs supportedPlatforms (system:
    let
      pkgs = import <nixpkgs> { inherit system; };
      haskellPackages = pkgs.lib.getAttrFromPath ["haskellPackages_${ghcVer}"] pkgs;
      Cabal = if pkgs.lib.versionOlder haskellPackages.ghcPlain.version "7.6" then haskellPackages.Cabal_1_16_0_3 else null;
      hackageDb = haskellPackages.hackageDb.override { inherit Cabal; };
    in
    haskellPackages.cabal.mkDerivation (self: {
      pname = "cabal2nix";
      src = cabal2nixSrc;
      version = cabal2nixSrc.gitTag;
      isLibrary = false;
      isExecutable = true;
      buildDepends = with haskellPackages; [ Cabal filepath hackageDb HTTP mtl regexPosix ];
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
