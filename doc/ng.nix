# Architecture of the cleaned-up Haskell packages set in Nixpkgs.

# run: rm -f /tmp/test ; nix-build -o /tmp/test ng.nix -A hsemail --show-trace && tree /tmp/test/

let

  pkgs = import <nixpkgs> {};
  lib = pkgs.lib;

  inherit (lib) callPackageWith;

  fix = f: let tmp = rec { x = f x; }; in tmp.x;

  extend = rattrs: f: self: let super = rattrs self; in super // f self super;

  hpkgs = self:
    let
      simplePackage = { name, buildInputs ? [], override, overrideDerivation, deepOverride }: pkgs.stdenv.mkDerivation {
        inherit name buildInputs;
        phases = ["installPhase"];
        installPhase = ''
          install -D /dev/null $out/$name
          for dir in $buildInputs $nativeBuildInputs $propagatedBuildInputs; do
            cp --no-clobber "$dir/"* $out/
          done
        '';
      };

      package = f: args: simplePackage (callPackageWith self f args);
    in
    {
      hsemail = package ({ mtl, parsec } : { name = "hsemail-0"; buildInputs = [mtl parsec]; }) { mtl = self.mtl2; };
      mtl1 = package ({ transformers } : { name = "mtl-1"; buildInputs = [transformers]; }) {};
      mtl2 = package ({ transformers } : { name = "mtl-2"; buildInputs = [transformers]; }) {};
      parsec = package ({ mtl } : { name = "parsec-0"; buildInputs = [mtl]; }) {};
      transformers = package ({} : { name = "transformers-0"; }) {};
    };

  defaultConfiguration = self: super: { mtl = self.mtl1; };

  otherConfiguration = self: super: { };

in

  fix (extend (extend hpkgs defaultConfiguration) otherConfiguration)
