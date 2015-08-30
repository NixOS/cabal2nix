# GNUmakefile

srcdir := $(PWD)

.PHONY: all check clean release hlint

all::		lens-construction-helper/lens-construction-helper.cabal \
		language-nix/language-nix.cabal \
		distribution-nixpkgs/distribution-nixpkgs.cabal \
		cabal2nix/cabal2nix.cabal \
		hackage2nix/hackage2nix.cabal \
		lens-construction-helper/cabal.sandbox.config \
		language-nix/cabal.sandbox.config \
		distribution-nixpkgs/cabal.sandbox.config \
		cabal2nix/cabal.sandbox.config \
		hackage2nix/cabal.sandbox.config \
		lens-construction-helper/dist/setup-config \
		language-nix/dist/setup-config \
		distribution-nixpkgs/dist/setup-config \
		cabal2nix/dist/setup-config \
		hackage2nix/dist/setup-config

check::		all
		cd lens-construction-helper && cabal test
		cd language-nix && cabal test
		cd distribution-nixpkgs && cabal test
		cd cabal2nix && cabal test
		cd hackage2nix && cabal test

release::	release-lens-construction-helper.nix \
		release-language-nix.nix \
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

lens-construction-helper/lens-construction-helper.cabal : lens-construction-helper/package.yaml
	cd $(dir $@) && hpack && cabal check
	touch $@

.sandbox/cabal.sandbox.config:
	mkdir $(dir $@) && cd $(dir $@) && cabal sandbox init && cabal sandbox add-source $(srcdir)/lens-construction-helper $(srcdir)/language-nix $(srcdir)/distribution-nixpkgs $(srcdir)/cabal2nix $(srcdir)/hackage2nix

lens-construction-helper/cabal.sandbox.config : .sandbox/cabal.sandbox.config
	cd $(dir $@) && cabal sandbox init --sandbox=$(srcdir)/.sandbox/.cabal-sandbox
	@touch $@

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

lens-construction-helper/dist/setup-config : lens-construction-helper/cabal.sandbox.config
	cd $(dir $<) && cabal install --dependencies-only && cabal configure --enable-tests
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

release-lens-construction-helper.nix : lens-construction-helper/lens-construction-helper.cabal cabal2nix/cabal.sandbox.config
	cd cabal2nix && cabal -v0 run cabal2nix -- --maintainer=simons $(srcdir)/lens-construction-helper >../$@

release-language-nix.nix : language-nix/language-nix.cabal cabal2nix/cabal.sandbox.config
	cd cabal2nix && cabal -v0 run cabal2nix -- --maintainer=simons $(srcdir)/language-nix >../$@

release-distribution-nixpkgs.nix : distribution-nixpkgs/distribution-nixpkgs.cabal cabal2nix/cabal.sandbox.config
	cd cabal2nix && cabal -v0 run cabal2nix -- --maintainer=simons $(srcdir)/distribution-nixpkgs >../$@

release-cabal2nix.nix : cabal2nix/cabal2nix.cabal cabal2nix/cabal.sandbox.config
	cd cabal2nix && cabal -v0 run cabal2nix -- --maintainer=simons $(srcdir)/cabal2nix >../$@

release-hackage2nix.nix : hackage2nix/hackage2nix.cabal cabal2nix/cabal.sandbox.config
	cd cabal2nix && cabal -v0 run cabal2nix -- --maintainer=simons $(srcdir)/hackage2nix >../$@
