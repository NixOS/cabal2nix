{ mkDerivation, ansi-wl-pprint, base, Cabal, containers
, distribution-nixpkgs, language-nix, lens
, lens-construction-helper, optparse-applicative, pretty, stdenv
}:
mkDerivation {
  pname = "cabal2nix";
  version = "20150830";
  src = /home/simons/src/cabal2nix/cabal2nix;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    ansi-wl-pprint base Cabal containers distribution-nixpkgs
    language-nix lens lens-construction-helper optparse-applicative
    pretty
  ];
  homepage = "https://github.com/nixos/cabal2nix#readme";
  description = "Convert Cabal files into Nix build instructions";
  license = stdenv.lib.licenses.bsd3;
  maintainers = with stdenv.lib.maintainers; [ simons ];
}
