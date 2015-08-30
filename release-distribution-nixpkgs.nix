{ mkDerivation, aeson, base, bytestring, Cabal, containers, deepseq
, deepseq-generics, directory, doctest, filepath, hackage-db, hspec
, language-nix, lens, lens-construction-helper, pretty, process
, SHA, split, stdenv, transformers, utf8-string
}:
mkDerivation {
  pname = "distribution-nixpkgs";
  version = "20150830";
  src = /home/simons/src/cabal2nix/distribution-nixpkgs;
  libraryHaskellDepends = [
    aeson base bytestring Cabal containers deepseq deepseq-generics
    directory doctest filepath hackage-db hspec language-nix lens
    lens-construction-helper pretty process SHA split transformers
    utf8-string
  ];
  testHaskellDepends = [
    aeson base bytestring Cabal containers deepseq deepseq-generics
    directory doctest filepath hackage-db hspec language-nix lens
    lens-construction-helper pretty process SHA split transformers
    utf8-string
  ];
  homepage = "https://github.com/nixos/cabal2nix#readme";
  description = "Convert Cabal files into Nix build instructions";
  license = stdenv.lib.licenses.bsd3;
  maintainers = with stdenv.lib.maintainers; [ simons ];
}
