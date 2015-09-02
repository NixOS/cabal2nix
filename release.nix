/* Build instructions for the continuous integration system Hydra. */

{ cabal2nixSrc ? { outPath = ./.; revCount = 0; gitTag = "dirty"; }
, releaseBuild ? false
, supportedSystems ? ["x86_64-linux"] ++ (if releaseBuild then ["i686-linux" "x86_64-darwin"] else [])
}:

with import <nixpkgs/pkgs/top-level/release-lib.nix> { inherit supportedSystems; };

let

  overrides = self: super: {

    mkJob = pkg: pkgs.haskell.lib.buildStrictly (super.${pkg}.override {
      mkDerivation = args: super.mkDerivation (args // { src = cabal2nixSrc; version = cabal2nixSrc.gitTag; });
    });

    lens-construction-helper = self.mkJob "lens-construction-helper";

    language-nix = self.mkJob "language-nix";

    distribution-nixpkgs = self.mkJob "distribution-nixpkgs";

    cabal2nix = self.mkJob "cabal2nix";

    hackage2nix = self.mkJob "hackage2nix";

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
