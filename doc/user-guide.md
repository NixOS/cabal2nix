---
title: User's Guide for Haskell in Nixpkgs
author: Peter Simons \<simons@cryp.to\>
date: 2015-05-21
---

# How to install Haskell packages

Nixpkgs distributes build instructions for all Haskell packages that are
registered on Hackage[^Hackage], but strangely enough those packages cannot be
discovered with the normal Nix package look-up:

    $ nix-env -qa cabal-install
    error: selector ‘cabal-install’ matches no derivations

    $ nix-env -i ghc
    error: selector ‘ghc’ matches no derivations

The reason for this strange behavior is that the Haskell package set is *huge*,
and if all Haskell packages were visible in the top-level package set, then
those kind of name-based search/install operations would be slowed down a lot.
So, instead of that, we keep Haskell-related packages in a separate attribute
set called `haskellPackages` which can be listed by running:

    $ nix-env -f "<nixpkgs>" -qaP -A haskellPackages
    haskellPackages.a50         a50-0.5
    haskellPackages.abacate     haskell-abacate-0.0.0.0
    haskellPackages.abcBridge   haskell-abcBridge-0.12
    haskellPackages.afv         afv-0.1.1
    haskellPackages.alex        alex-3.1.4
    haskellPackages.Allure      Allure-0.4.101.1
    haskellPackages.alms        alms-0.6.7
    [... some 8000 entries omitted  ...]

Haskell packages who's Nix name (second column) begins with a `haskell-` prefix
are packages that provide a library whereas packages without that prefix
provide just executables. (Libraries may provide executables too, though. The
package `haskell-pandoc`, for example, installs both a library and an
application.) You can install and use Haskell executables just like any other
program in Nixpkgs, but using Haskell libraries for development is a bit
trickier and we'll address that subject in great detail in the next section.

To install any of those packages into your profile, refer to them by attribute
path (first column):

    $ nix-env -f "<nixpkgs>" -iA haskellPackages.Allure ...

The name of every Haskell packages' attribute corresponds exactly to the name
of the package on Hackage. The package `cabal-install` has the attribute
`haskellPackages.cabal-install`, and so
on.[^names-that-cannot-be-mapped-to-attributes]

Attribute paths may vary depending on they way your system is set up. We dodged
that problem so far by giving `nix-env` an explicit `-f "<nixpkgs>"` parameter,
but if you call `nix-env` without that flag, then chances are the invocation
fails:

    $ nix-env -iA haskellPackages.cabal-install --dry-run
    error: attribute ‘haskellPackages’ in selection path
           ‘haskellPackages.cabal-install’ not found

The `-f "<nixpkgs>"` argument ensures that the top-level namespace visible to
`nix-env` is the Nixpkgs package set. Without that parameter, however, this is
not necessarily the case. On NixOS, for example, Nixpkgs does by default *not*
live in the top-level namespace. The easiest way to figure out the proper
attribute path is to query for the path of a well-known Nixpkgs package, i.e.:

    $ nix-env -qaP coreutils
    nixos.pkgs.coreutils  coreutils-8.23

If your system responds like that (which most NixOS installatios will), then
the attribute path to `haskellPackages` is `nixos.pkgs.haskellPackages`, and
it's also possible to run `nix-env` as follows, ommitting the `-f` flag:

    $ nix-env -qaP -A nixos.pkgs.haskellPackages
    $ nix-env -iA nixos.pkgs.haskellPackages.cabal-install

## Choosing packages built with different compilers

Nixpkgs contains the latest major release of every GHC since 6.10.4, and
consequently there is not just one Haskell package set --- there is one per
compiler:

    $ nix-env -f "<nixpkgs>" -qaP -A haskell.packages.ghc6123
    $ nix-env -f "<nixpkgs>" -qaP -A haskell.packages.ghc763
    $ nix-env -f "<nixpkgs>" -qaP -A haskell.packages.ghc7101

The name `haskellPackages` is just a synonym for `haskell.packages.ghc7101`,
which is our recommended default package set at the moment, but ultimately you
are free to compile your Haskell packages with any GHC version you please. Run
the following command to display the complete list of available compilers:

    $ nix-env -f "<nixpkgs>" -qaP -A haskell.compiler
    haskell.compiler.ghc6104        ghc-6.10.4
    haskell.compiler.ghc6123        ghc-6.12.3
    haskell.compiler.ghc704         ghc-7.0.4
    haskell.compiler.ghc722         ghc-7.2.2
    haskell.compiler.ghc742         ghc-7.4.2
    haskell.compiler.ghc763         ghc-7.6.3
    haskell.compiler.ghc784         ghc-7.8.4
    haskell.compiler.ghc7101        ghc-7.10.1
    haskell.compiler.ghcHEAD        ghc-7.11.20150402
    haskell.compiler.ghcjs          ghcjs-0.1.0
    haskell.compiler.jhc            jhc-0.8.2
    haskell.compiler.uhc            uhc-1.1.9.0

For every compiler version *XYZ*, the attributes `haskell.compiler.ghcXYC` and
`haskell.packages.ghcXYC.ghc` are synonymous.

## Install more than one Haskell compiler

### temporary

    $ nix-shell -p haskell.compiler.ghc7101
    [nix-shell:~]$ ghc --numeric-version
    7.10.1
    [nix-shell:~]$ exit

    $ nix-shell -p haskell.compiler.ghc784 \
                --command "cabal configure"
    $ cabal build

### transient

    $ nix-env -f "<nixpkgs>" -p ~/ghc-7.6.3 -iA haskell.compiler.ghc763
    $ nix-env -f "<nixpkgs>" -p ~/ghc-7.8.4 -iA haskell.compiler.ghc784
    [...]

    $ export PATH=$HOME/ghc-7.6.3/bin:$PATH
    $ ghc --numeric-version
    7.6.3

    $ cabal configure --with-compiler=$HOME/ghc-7.6.3/bin/ghc

### permanent

    $ PROFILE_DIR=/nix/var/nix/profiles/per-user/$USER
    $ nix-env -f "<nixpkgs>" -p $PROFILE_DIR/ghc-7.6.3 -iA ...

On NixOS, `/etc/profile` defines `$NIX_USER_PROFILE_DIR` automatically.

# Create a Haskell development environment

Edit the file `~/.nixpkgs/config.nix`:

      {
        packageOverrides = super: let self = super.pkgs; in
        {
          myHaskellEnv =
            self.haskell.packages.ghc7101.ghcWithPackages
              (haskellPackages: with haskellPackages; [
                arrows async cabal-install case-insensitive
                cgi criterion hspec HStringTemplate
              ]);
        };
      }

Now installl with `nix-env -f "<nixpkgs>" -iA myHaskellEnv`.

The generated `ghc` program is a wrapper script that re-directs the real
`ghc` to use a "libdir" with all the specified packages installed:

      #! /nix/store/xlxjcjbnbwnz...-bash-4.3-p33/bin/bash -e
      realGhc=/nix/store/zzhasddj77xhwdban95...-ghc-7.10.1
      ghcEnv=/nix/store/cgddwzz9hkdgprvbymph...-ghc-7.10.1
      export NIX_GHC=$ghcEnv/bin/ghc
      export NIX_GHCPKG=$ghcEnv/bin/ghc-pkg
      export NIX_GHC_DOCDIR=$ghcEnv/share/doc/ghc/html
      export NIX_GHC_LIBDIR=$ghcEnv/lib/ghc-7.10.1
      exec $realGhc/bin/ghc "-B$NIX_GHC_LIBDIR" "${extraFlagsArray[@]}" "$@"


Define the same environment variables in your `~/.bashrc`:

      NIX_GHC="$HOME/.nix-profile/bin/ghc"
      ghcVersion=$($NIX_GHC --numeric-version)
      NIX_GHCPKG="$HOME/.nix-profile/bin/ghc-pkg"
      NIX_GHC_DOCDIR="$HOME/.nix-profile/share/doc/ghc/html"
      NIX_GHC_LIBDIR="$HOME/.nix-profile/lib/ghc-$ghcVersion"
      export NIX_GHC NIX_GHCPKG NIX_GHC_DOCDIR NIX_GHC_LIBDIR
      unset ghcVersion

Or:

      if [ -e ~/.nix-profile/bin/ghc ]; then
        eval $(grep export ~/.nix-profile/bin/ghc)
      fi

## Ad hoc environments

Save as `shell.nix`:

      with (import <nixpkgs> {}).pkgs;
      let
        ghc = haskell.packages.ghc7101.ghcWithPackages
                (pkgs: with pkgs; [ aeson lens monad-par ]);
      in
      stdenv.mkDerivation {
        name = "my-haskell-env-0";
        buildInputs = [ ghc ];
        shellHook = "eval $(grep export ${ghc}/bin/ghc)";
      }

Now run `nix-shell`, or even `nix-shell --pure`.

To parameterize the compiler version, edit the file as follows:

      { compiler ? "ghc7101" }:

      with (import <nixpkgs> {}).pkgs;
      let
        ghc = haskell.packages.${compiler}.ghcWithPackages
                (pkgs: with pkgs; [ aeson lens monad-par ]);
      in
      [...]

Now run "`nix-shell --argstr compiler ghc784`" to select a different
compiler.

Every Haskell package has an `env` attribute that provides a temporary
shell environment in which that package can be built:

      $ cabal get lens && cd lens-4.10
      Downloading lens-4.10...
      Unpacking to lens-4.10/

      $ nix-shell "<nixpkgs>" -A haskellPackages.lens.env
      [nix-shell:/tmp/lens-4.10]$ cabal configure
      Resolving dependencies...
      Configuring lens-4.10...
      [...]

# Adding support for your own packages

## For `nix-shell`

    $ nix-env -f "<nixpkgs>" -iA cabal2nix

    $ cabal2nix --shell . >shell.nix
    $ nix-shell --command "cabal configure"

    $ cabal build
    [...]

    $ cabal2nix --shell .
    with (import <nixpkgs> {}).pkgs;
    let pkg = haskellPackages.callPackage
                ({ mkDerivation, base, stdenv, transformers }:
                 mkDerivation {
                   pname = "mtl";
                   version = "2.2.1";
                   src = ./.;
                   buildDepends = [ base transformers ];
                   homepage = "http://github.com/ekmett/mtl";
                   license = stdenv.lib.licenses.bsd3;
                 }) {};
    in
      pkg.env

      $ cd ~/src/foo
      $ cabal2nix . > default.nix

Now edit `~/.nixpkgs/config.nix`:

      {
        packageOverrides = super: let self = super.pkgs; in
        {
          foo = self.haskellPackages.callPackage
                  ../src/foo {};
        };
      }

Build it by running "`nix-env -f "<nixpkgs>" -iA foo`".

## For `nix-build`

# Create builds for your own packages

Use this `~/.nixpkgs/config.nix` file to register the build inside of
`haskellPackages`, so that other libraries may depend on it:

      {
        packageOverrides = super: let self = super.pkgs; in
        {
          haskellPackages = super.haskellPackages.override {
            overrides = self: super: {
              foo = self.callPackage ../src/foo {};
              bar = self.callPackage ../src/bar {};
            };
          };
        };
      }

# How to download binary packages

NixOS users add this setting to `/etc/nixos/configuration.nix`:

      nix.trustedBinaryCaches = [
        http://hydra.nixos.org
      ];

Everyone else adds this setting to `/etc/nix/nix.conf`:

      trusted-binary-caches = http://hydra.nixos.org

Now run `nix-env -f "<nixpkgs>"`, `nix-build`, and `nix-shell` with this option:

      --option extra-binary-caches http://hydra.nixos.org

## GHC's infamous non-deterministic library ID bug

GHC and distributed build farms don't get along well:

      https://ghc.haskell.org/trac/ghc/ticket/4012
      https://github.com/NixOS/nixpkgs/issues/7792

When you see an error like this one

      package foo-0.7.1.0 is broken due to missing package
      text-1.2.0.4-98506efb1b9ada233bb5c2b2db516d91

then you have to garbage collect `foo` and all its dependents,
and re-install from scratch.

      # nix-store -q --referrers /nix/store/*-haskell-text-1.2.0.4 | nix-store --repair-path --option binary-caches http://hydra.nixos.org

If you're using a Hydra server other than `hydra.nixos.org`, then it
might be necessary to disable the binary channels for the duration of
the previous command, i.e. by running:

      # nix-channel --remove nixos




[^Hackage]: http://hackage.haskell.org/

[^names-that-cannot-be-mapped-to-attributes]: This convention causes trouble
with packages like `3dmodels` and `4Blocks`, because these names are invalid
identifiers in the Nix language. The issue of how to deal with these (rare)
corner cases is currently unresolved.
