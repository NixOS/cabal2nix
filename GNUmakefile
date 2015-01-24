# GNUmakefile

.PHONY: all configure build check check

all::		check

configure::	cabal2nix.cabal
	@cabal configure --ghc-option=-j --enable-tests

build::		configure
	@cabal build -j

check::		build
	@cabal test -j

cabal2nix.cabal:	generate-cabal-file.hs
	runhaskell "$<" >"$@"
	cabal check
