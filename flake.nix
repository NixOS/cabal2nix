{
  description = "Generate Nix build instructions from a Cabal file";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system: let

      pkgs = nixpkgs.legacyPackages.${system};

      ghc8107 = pkgs.haskell.packages."ghc8107";

      # modifier used in haskellPackages.developPackage
      myModifier = hsPkgs: drv:
        pkgs.haskell.lib.addBuildTools drv ([
          hsPkgs.cabal-install
          hsPkgs.haskell-language-server
        ]);

      mkPackage = hsPkgs:
          hsPkgs.developPackage {
            root =  pkgs.lib.cleanSource ./. ;
            name =  "cabal2nix";
            returnShellEnv = false;
            withHoogle = true;
            modifier = myModifier hsPkgs;
          };

    in {
      packages = {
        cabal2nix = self.packages.${system}.cabal2nix-8107;
        cabal2nix-8107 = mkPackage ghc8107;
      };

      devShell = self.packages.${system}.cabal2nix.envFunc {};

      defaultPackage = self.packages.${system}.cabal2nix;
  });
}
