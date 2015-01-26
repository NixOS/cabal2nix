# GNUmakefile

.PHONY: all configure build check haddock hlint

all::		check

configure::	cabal2nix.cabal
	@cabal configure --ghc-option=-j --enable-tests

build::		configure
	@cabal build -j

haddock::	build
	@cabal haddock

check::		haddock
	@cabal test -j

hlint::
	hlint src

cabal2nix.cabal:	generate-cabal-file.hs
	runhaskell "$<" >"$@"
	cabal check
