# Architecture of the cleaned-up Haskell packages set in Nixpkgs.

# run: rm -f /tmp/test ; nix-build -o /tmp/test ng.nix -A hsemail --show-trace && tree /tmp/test/

let

  pkgs = import <nixpkgs> {};
  lib = pkgs.lib;

  inherit (pkgs) newScope;

  fix = f: let x = f x // { __unfix__ = f; }; in x;

  extend = rattrs: f: self: let super = rattrs self; in super // f self super;

  hpkgs = self:
    let

      _buildCabal = { name, buildInputs ? [], ... }: pkgs.stdenv.mkDerivation {
        inherit name buildInputs;
        phases = ["installPhase"];
        installPhase = ''
          install -D /dev/null $out/$name
          for dir in $buildInputs $nativeBuildInputs $propagatedBuildInputs; do
            cp --no-clobber "$dir/"* $out/
          done
        '';
      };

      buildCabal = pkg: pkg; #{ name, buildInputs ? [], ... }: { inherit name buildInputs; };

      definePackage = pkg: overrides: buildCabal (newScope (fix (extend (self.__unfix__) overrides)) pkg {});

    in
    {
      hsemail = definePackage ({ mtl, parsec } : { name = "hsemail-0"; buildInputs = [mtl parsec]; }) (self: super: {});
      mtl1 = definePackage ({ transformers } : { name = "mtl-1"; buildInputs = [transformers]; }) (self: super: {});
      mtl2 = definePackage ({ transformers } : { name = "mtl-2"; buildInputs = [transformers]; }) (self: super: {});
      mtl3 = definePackage ({ transformers } : { name = "mtl-3"; buildInputs = [transformers]; }) (self: super: {});
      mtl = self.mtl1;
      parsec = definePackage ({ mtl } : { name = "parsec-0"; buildInputs = [mtl]; }) (self: super: {});
      transformers = definePackage ({} : { name = "transformers-0"; }) (self: super: {});
    };

  defaultConfiguration = self: super: { };

  otherConfiguration = self: super: { };

  haskellPackages = fix (extend (extend hpkgs defaultConfiguration) otherConfiguration);
in

  haskellPackages # .hsemail.deepOverride { mtl = haskellPackages.mtl2; }
