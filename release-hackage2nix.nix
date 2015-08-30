{ mkDerivation, base, Cabal, containers, distribution-nixpkgs
, filepath, language-nix, lens, monad-par, monad-par-extras, mtl
, optparse-applicative, pretty, stdenv
}:
mkDerivation {
  pname = "hackage2nix";
  version = "20150830";
  src = /home/simons/src/cabal2nix/hackage2nix;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base Cabal containers distribution-nixpkgs filepath language-nix
    lens monad-par monad-par-extras mtl optparse-applicative pretty
  ];
  homepage = "https://github.com/nixos/cabal2nix#readme";
  description = "Convert Cabal files into Nix build instructions";
  license = stdenv.lib.licenses.bsd3;
  maintainers = with stdenv.lib.maintainers; [ simons ];
}
