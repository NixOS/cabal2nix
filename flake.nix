{ inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    flake-utils.url = "github:numtide/flake-utils/v1.0.0";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        ghcVersion = "ghc90";

        config = { };

        overlay = self: super: {
          haskell = super.haskell // {
            packages = super.haskell.packages // {
              "${ghcVersion}" = super.haskell.packages."${ghcVersion}".override (old: {
                overrides =
                  self.lib.composeExtensions
                    (old.overrides or (_: _: { }))
                    (hself: hsuper: {
                      # The reason we stick these underneath a `local` attribute
                      # is to avoid infinite recursion (because `callCabal2nix`
                      # depends on all of these packages.
                      local = rec {
                        cabal2nix =
                          hself.callCabal2nix "cabal2nix" ./cabal2nix {
                            inherit distribution-nixpkgs hackage-db;
                          };

                        distribution-nixpkgs =
                          hself.callCabal2nix
                            "distribution-nixpkgs"
                            ./distribution-nixpkgs
                            { inherit language-nix; };

                        hackage-db =
                          hself.callCabal2nix "hackage-db" ./hackage-db { };

                        language-nix =
                          hself.callCabal2nix "language-nix" ./language-nix { };
                      };
                    });
              });
            };
          };
        };

        pkgs =
          import nixpkgs { inherit config system; overlays = [ overlay ]; };

      in
        rec {
          packages = rec {
            inherit (pkgs.haskell.packages."${ghcVersion}".local)
              cabal2nix
              hackage-db
              distribution-nixpkgs
              language-nix
            ;

            default = cabal2nix;
          };

          devShells = rec {
            cabal2nix =
              pkgs.haskell.packages."${ghcVersion}".local.cabal2nix.env;

            distribution-nixpkgs =
              pkgs.haskell.packages."${ghcVersion}".local.distribution-nixpkgs.env;

            hackage-db =
              pkgs.haskell.packages."${ghcVersion}".local.hackage-db.env;

            language-nix =
              pkgs.haskell.packages."${ghcVersion}".local.language-nix.env;

            default = cabal2nix;
          };
        }
    );
}
