---
title: User's Guide for Haskell in Nixpkgs
author: Peter Simons \<simons@cryp.to\>
date: 2015-05-21
---

# How to install Haskell packages

Nixpkgs distributes build instructions for all Haskell packages registered on
Hackage[^Hackage], but strangely enough normal Nix package lookups don't seem
to discover any of them:

    $ nix-env -qa cabal-install
    error: selector ‘cabal-install’ matches no derivations

    $ nix-env -i ghc
    error: selector ‘ghc’ matches no derivations

The Haskell package set is not registered in the top-level namespace because it
is *huge*. If all Haskell packages were visible to these commands, then
name-based search/install operations would be much slower than they are now. We
avoided that by keeping all Haskell-related packages in a separate attribute
set called `haskellPackages`, which the following command will list:

    $ nix-env -f "<nixpkgs>" -qaP -A haskellPackages
    haskellPackages.a50         a50-0.5
    haskellPackages.abacate     haskell-abacate-0.0.0.0
    haskellPackages.abcBridge   haskell-abcBridge-0.12
    haskellPackages.afv         afv-0.1.1
    haskellPackages.alex        alex-3.1.4
    haskellPackages.Allure      Allure-0.4.101.1
    haskellPackages.alms        alms-0.6.7
    [... some 8000 entries omitted  ...]

To install any of those packages into your profile, refer to them by their
attribute path (first column):

    $ nix-env -f "<nixpkgs>" -iA haskellPackages.Allure ...

The attribute path of any Haskell packages corresponds to the name of that
particular package on Hackage: the package `cabal-install` has the attribute
`haskellPackages.cabal-install`, and so
on.[^names-that-cannot-be-mapped-to-attributes]

Haskell packages who's Nix name (second column) begins with a `haskell-` prefix
are packages that provide a library whereas packages without that prefix
provide just executables. Libraries may provide executables too, though: the
package `haskell-pandoc`, for example, installs both a library and an
application. You can install and use Haskell executables just like any other
program in Nixpkgs, but using Haskell libraries for development is a bit
trickier and we'll address that subject in great detail in section [How to
create a development environment].

Attribute paths are deterministic inside of Nixpkgs, but the path necessary to
reach Nixpkgs varies from system to system. We dodged that problem by giving
`nix-env` an explicit `-f "<nixpkgs>"` parameter, but if you call `nix-env`
without that flag, then chances are the invocation fails:

    $ nix-env -iA haskellPackages.cabal-install
    error: attribute ‘haskellPackages’ in selection path
           ‘haskellPackages.cabal-install’ not found

On NixOS, for example, Nixpkgs does *not* exist in the top-level namespace by
default. To figure out the proper attribute path, it's easiest to query for the
path of a well-known Nixpkgs package, i.e.:

    $ nix-env -qaP coreutils
    nixos.pkgs.coreutils  coreutils-8.23

If your system responds like that (most NixOS installatios will), then the
attribute path to `haskellPackages` is `nixos.pkgs.haskellPackages`. Thus, if
you want to use `nix-env` without giving an explicit `-f` flag, then that's the
way to do it:

    $ nix-env -qaP -A nixos.pkgs.haskellPackages
    $ nix-env -iA nixos.pkgs.haskellPackages.cabal-install

Our current default compiler is GHC 7.10.x and the `haskellPackages` set
contains packages built with that particular version. Nixpkgs contains the
latest major release of every GHC since 6.10.4, however, and there is a whole
family of package sets available that defines Hackage packages built with each
of those compilers, too:

    $ nix-env -f "<nixpkgs>" -qaP -A haskell.packages.ghc6123
    $ nix-env -f "<nixpkgs>" -qaP -A haskell.packages.ghc763

The name `haskellPackages` is really just a synonym for
`haskell.packages.ghc7101`, because we prefer that package set internally and
recommend it to our users as their default choice, but ultimately you are free
to compile your Haskell packages with any GHC version you please. The following
command displays the complete list of available compilers:

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

There exists a package set for every GHC version in that list. There are no
package sets for `jhc`, `uhc`, or any other non-`ghc` Haskell compiler,
unfortunately, because no-one figured out how to build Cabal packages with them
yet. It's probably no big deal to accomplish, but someone has to do it. Also,
the attributes `haskell.compiler.ghcXYC` and `haskell.packages.ghcXYC.ghc` are
synonymous for the sake of convenience.

# How to create a development environment

## How to install a compiler

A simple development environment consists of a Haskell compiler and the tool
`cabal-install`, and we saw in section [How to install Haskell packages] how
you can install those programs into your user profile:

    $ nix-env -f "<nixpkgs>" -iA haskellPackages.ghc haskellPackages.cabal-install

Instead of the default package set `haskellPackages`, you can also use the more
precise name `haskell.compiler.ghc7101`, which has the advantage that it refers
to the same GHC version regardless of what Nixpkgs considers "default" at any
given time.

Once you've made those tools available in `$PATH`, it's possible to build
Hackage packages the same way people without access to Nix do it all the time:

    $ cabal get lens-4.11 && cd lens-4.11
    $ cabal install -j --dependencies-only
    $ cabal configure
    $ cabal build

If you enjoy working with Cabal sandboxes, then that's entirely possible too:
just execute the command

    $ cabal sandbox init

before installing the required dependencies.

The `nix-shell` utility makes it easy to switch to a different compiler
version; just enter the Nix shell environment with the command

    $ nix-shell -p haskell.compiler.ghc784

to bring GHC 7.8.4 into `$PATH`. Re-running `cabal configure` switches your
build to use that compiler instead. If you're working on a project that doesn't
depend on any additional system libraries outside of GHC, then it's sufficient
even to run the `cabal configure` command inside of the shell:

    $ nix-shell -p haskell.compiler.ghc784 --command "cabal configure"

Afterwards, all other commands like `cabal build` work just fine in any shell
environment, because the configure phase recorded the absolute paths to all
required tools like GHC in its build configuration inside of the `dist/`
directory. Please note, however, that `nix-collect-garbage` can break such an
environment because the Nix store paths created by `nix-shell` aren't "alive"
anymore once `nix-shell` has terminated. If you find that your Haskell builds
no longer work after garbage collection, then you'll have to re-run `cabal
configure` inside of a new `nix-shell` environment.

## How to install a compiler with libraries

Edit the file `~/.nixpkgs/config.nix`:

    {
      packageOverrides = super: let self = super.pkgs; in
      {
        myHaskellEnv = self.haskell.packages.ghc7101.ghcWithPackages
                         (haskellPackages: with haskellPackages; [
                           # libraries
                           arrows async cgi criterion
                           # tools
                           cabal-install haskintex
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
