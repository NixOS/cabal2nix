# GNUmakefile

.PHONY: all configure re-configure build check haddock hlint clean

all::		check

configure::	cabal2nix.cabal
	@cabal -v0 configure --ghc-option=-j --enable-tests

re-configure::
	@rm -f cabal2nix.cabal
	@$(MAKE) configure

build::		configure
	@cabal build -j

haddock::	build
	@cabal haddock

check::		haddock
	@cabal test -j

hlint::
	hlint src

clean::
	@rm -rf cabal2nix.cabal dist src/Internal/Version.hs test/doctest.hs

cabal2nix.cabal:	generate-cabal-file.hs
	runhaskell "$<" >"$@"
	@cabal check
