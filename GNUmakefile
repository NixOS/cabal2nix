# GNUmakefile

.PHONY: all configure build check haddock hlint clean

all::		check

configure::	cabal2nix.cabal
	@cabal -v0 configure --ghc-option=-j --enable-tests

build::		configure
	@cabal build -j

haddock::	build
	@cabal haddock

check::		haddock
	@cabal test -j

hlint::
	hlint src

clean::
	@rm -rf cabal2nix.cabal dist src/Cabal2Nix/Version.hs test/doctest.hs

cabal2nix.cabal:	generate-cabal-file.hs
	runhaskell "$<" >"$@"
	cabal check
