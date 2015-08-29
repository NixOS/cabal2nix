# GNUmakefile

.PHONY: all configure build check haddock hlint clean packunused

all::		check

configure::	cabal2nix.cabal
	@cabal -v0 configure --ghc-option=-j --enable-tests

build::		configure
	@cabal build -j

check::		haddock
	@cabal test -j

haddock::	build
	@cabal haddock --hyperlink-source

hlint::
	hlint src

packunused::
	@cabal clean
	@find . -name "*.imports" -exec rm {} +
	@cabal configure -O0 --disable-library-profiling
	@cabal build --ghc-option=-ddump-minimal-imports
	@packunused

clean::
	@rm -rf dist TAGS

cabal2nix.cabal : package.yaml
	@hpack >/dev/null
