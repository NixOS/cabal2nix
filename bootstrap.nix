with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, cartel, mtl, process, stdenv }:
             mkDerivation {
               pname = "generate-cabal-file";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = false;
               isExecutable = true;
               buildDepends = [ base cartel mtl process ];
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkgs.lib.overrideDerivation pkg.env (drv: {
    shellHook = ''
      ${drv.shellHook}
      make cabal2nix.cabal
      exit 1
    '';
  })
