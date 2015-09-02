{ mkDerivation, ansi-wl-pprint, base, Cabal, containers
, distribution-nixpkgs, language-nix, lens
, lens-construction-helper, optparse-applicative, pretty
, pretty-show, stdenv, fetchFromGitHub
}:

mkDerivation rec {
  pname = "cabal2nix";
  version = "20150824-66-gd281a60";
  src = fetchFromGitHub {
    owner = "nixos";
    repo = "cabal2nix";
    rev = "v${version}";
    sha256 = "1ffizg60ihkipcgqr5km4vxgnqv2pdw4716amqlxgf31wj59nyas";
  };
  postUnpack = "sourceRoot+=/${pname}";
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    ansi-wl-pprint base Cabal containers distribution-nixpkgs
    language-nix lens lens-construction-helper optparse-applicative
    pretty pretty-show
  ];
  homepage = "https://github.com/nixos/cabal2nix#readme";
  description = "Convert Cabal files into Nix build instructions";
  license = stdenv.lib.licenses.bsd3;
  maintainers = with stdenv.lib.maintainers; [ simons ];
}
