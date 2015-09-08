# GNUmakefile

srcdir := $(CURDIR)

.PHONY: all clean hlint

all::		language-nix/language-nix.cabal \
		distribution-nixpkgs/distribution-nixpkgs.cabal \
		cabal2nix/cabal2nix.cabal \
		hackage2nix/hackage2nix.cabal

hlint::
	hlint */src */test

cabal2nix/cabal2nix.cabal : cabal2nix/package.yaml
	cd $(dir $@) && hpack && cabal check
	touch $@

distribution-nixpkgs/distribution-nixpkgs.cabal : distribution-nixpkgs/package.yaml
	cd $(dir $@) && hpack && cabal check
	touch $@

hackage2nix/hackage2nix.cabal : hackage2nix/package.yaml
	cd $(dir $@) && hpack && cabal check
	touch $@

language-nix/language-nix.cabal : language-nix/package.yaml
	cd $(dir $@) && hpack && cabal check
	touch $@

clean::
	@rm -f */*.cabal
