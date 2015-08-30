{ mkDerivation, base, data-default-class, lens, stdenv }:
mkDerivation {
  pname = "lens-construction-helper";
  version = "20150830";
  src = /home/simons/src/cabal2nix/lens-construction-helper;
  libraryHaskellDepends = [ base data-default-class lens ];
  homepage = "https://github.com/nixos/cabal2nix#readme";
  description = "Use data-default to create default instances of various types";
  license = stdenv.lib.licenses.bsd3;
  maintainers = with stdenv.lib.maintainers; [ simons ];
}
