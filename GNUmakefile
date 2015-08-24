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
	@cabal haddock --hyperlink-source

check::		haddock
	@cabal test -j

hlint::
	hlint src

clean::
	@rm -rf cabal2nix.cabal dist src/Internal/Version.hs test/doctest.hs TAGS

cabal2nix.cabal:	generate-cabal-file.hs
	runhaskell "$<" >"$@"
	@cabal check
