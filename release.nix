/* Build instructions for the continuous integration system Hydra. */

{ cabal2nixSrc ? { outPath = ./.; revCount = 0; gitTag = "dirty"; }
, releaseBuild ? false
, supportedSystems ? ["x86_64-linux"] ++ (if releaseBuild then ["i686-linux" "x86_64-darwin"] else [])
}:

with import <nixpkgs/pkgs/top-level/release-lib.nix> { inherit supportedSystems; };

let

  overrides = self: super: {

    mkDerivationFor = subdir: args: self.mkDerivation (args // {
      src = cabal2nixSrc; version = cabal2nixSrc.gitTag;
      postUnpack = "sourceRoot+=/${subdir}";
    });

    mkJob = path: subdir: pkgs.haskell.lib.buildStrictly (self.callPackage path { mkDerivation = self.mkDerivationFor subdir; });

    lens-construction-helper = self.mkJob ./release-lens-construction-helper.nix "lens-construction-helper";

    language-nix = self.mkJob ./release-language-nix.nix "language-nix";

    distribution-nixpkgs = self.mkJob ./release-distribution-nixpkgs.nix "distribution-nixpkgs";

    cabal2nix = self.mkJob ./release-cabal2nix.nix "cabal2nix";

    hackage2nix = self.mkJob ./release-hackage2nix.nix "hackage2nix";

  };

  mkJob = attr: testOn supportedSystems (pkgs: ((pkgs.haskellPackages.override { inherit overrides; })).${attr});

in
{

  lens-construction-helper = mkJob "lens-construction-helper";

  language-nix = mkJob "language-nix";

  distribution-nixpkgs = mkJob "distribution-nixpkgs";

  cabal2nix = mkJob "cabal2nix";

  hackage2nix = mkJob "hackage2nix";

}
