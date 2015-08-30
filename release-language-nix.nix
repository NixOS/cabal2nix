{ mkDerivation, base, data-default-class, deepseq-generics, lens
, lens-construction-helper, pretty, regex-posix, stdenv
}:
mkDerivation {
  pname = "language-nix";
  version = "20150830";
  src = /home/simons/src/cabal2nix/language-nix;
  libraryHaskellDepends = [
    base data-default-class deepseq-generics lens
    lens-construction-helper pretty regex-posix
  ];
  homepage = "https://github.com/nixos/cabal2nix#readme";
  description = "Data types and useful functions to represent and manipulate the Nix language";
  license = stdenv.lib.licenses.bsd3;
  maintainers = with stdenv.lib.maintainers; [ simons ];
}
