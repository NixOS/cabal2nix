# GNUmakefile

srcdir := $(CURDIR)

.PHONY: all check clean release hlint

all::		language-nix/dist/setup-config \
		distribution-nixpkgs/dist/setup-config \
		cabal2nix/dist/setup-config \
		hackage2nix/dist/setup-config

check::		all
		cd language-nix && cabal test
		cd distribution-nixpkgs && cabal test
		cd cabal2nix && cabal test
		cd hackage2nix && cabal test

release::	release-language-nix.nix \
		release-distribution-nixpkgs.nix \
		release-cabal2nix.nix \
		release-hackage2nix.nix \

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

.sandbox/cabal.sandbox.config:	language-nix/language-nix.cabal \
				distribution-nixpkgs/distribution-nixpkgs.cabal \
				cabal2nix/cabal2nix.cabal \
				hackage2nix/hackage2nix.cabal
	mkdir $(dir $@) && cd $(dir $@) && cabal sandbox init && cabal sandbox add-source $(srcdir)/language-nix $(srcdir)/distribution-nixpkgs $(srcdir)/cabal2nix $(srcdir)/hackage2nix

language-nix/cabal.sandbox.config : .sandbox/cabal.sandbox.config
	cd $(dir $@) && cabal sandbox init --sandbox=$(srcdir)/.sandbox/.cabal-sandbox
	@touch $@

distribution-nixpkgs/cabal.sandbox.config : .sandbox/cabal.sandbox.config
	cd $(dir $@) && cabal sandbox init --sandbox=$(srcdir)/.sandbox/.cabal-sandbox
	@touch $@

cabal2nix/cabal.sandbox.config : .sandbox/cabal.sandbox.config
	cd $(dir $@) && cabal sandbox init --sandbox=$(srcdir)/.sandbox/.cabal-sandbox
	@touch $@

hackage2nix/cabal.sandbox.config : .sandbox/cabal.sandbox.config
	cd $(dir $@) && cabal sandbox init --sandbox=$(srcdir)/.sandbox/.cabal-sandbox
	@touch $@

language-nix/dist/setup-config : language-nix/cabal.sandbox.config
	cd $(dir $<) && cabal install --dependencies-only && cabal configure --enable-tests
	@touch $@

distribution-nixpkgs/dist/setup-config : distribution-nixpkgs/cabal.sandbox.config
	cd $(dir $<) && cabal install --dependencies-only && cabal configure --enable-tests
	@touch $@

cabal2nix/dist/setup-config : cabal2nix/cabal.sandbox.config
	cd $(dir $<) && cabal install --dependencies-only && cabal configure --enable-tests
	@touch $@

hackage2nix/dist/setup-config : hackage2nix/cabal.sandbox.config
	cd $(dir $<) && cabal install --dependencies-only && cabal configure --enable-tests
	@touch $@

release-language-nix.nix : language-nix/language-nix.cabal cabal2nix/cabal.sandbox.config
	cd cabal2nix && cabal -v0 run cabal2nix -- --maintainer=simons $(srcdir)/language-nix >../$@

release-distribution-nixpkgs.nix : distribution-nixpkgs/distribution-nixpkgs.cabal cabal2nix/cabal.sandbox.config
	cd cabal2nix && cabal -v0 run cabal2nix -- --maintainer=simons $(srcdir)/distribution-nixpkgs >../$@

release-cabal2nix.nix : cabal2nix/cabal2nix.cabal cabal2nix/cabal.sandbox.config
	cd cabal2nix && cabal -v0 run cabal2nix -- --maintainer=simons $(srcdir)/cabal2nix >../$@

release-hackage2nix.nix : hackage2nix/hackage2nix.cabal cabal2nix/cabal.sandbox.config
	cd cabal2nix && cabal -v0 run cabal2nix -- --maintainer=simons $(srcdir)/hackage2nix >../$@
