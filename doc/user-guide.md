---
title: User's Guide for Haskell in Nixpkgs
author: Peter Simons
date: 2015-06-01
---

# How to install Haskell packages

Nixpkgs distributes build instructions for all Haskell packages registered on
[Hackage](http://hackage.haskell.org/), but strangely enough normal Nix package
lookups don't seem to discover any of them:

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
`haskellPackages.cabal-install`, and so on. (Actually, this convention causes
trouble with packages like `3dmodels` and `4Blocks`, because these names are
invalid identifiers in the Nix language. The issue of how to deal with these
rare corner cases is currently unresolved.)

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

GHC expects to find all installed libraries inside of its own `lib` directory.
This approach works fine on traditional Unix systems, but it doesn't work for
Nix, because GHC's store path is immutable once it's built. We cannot install
additional libraries into that location. As a consequence, our copies of GHC
don't know any packages except their own core libraries, like `base`,
`containers`, `Cabal`, etc.

We can register additional libraries to GHC, however, using a special build
function called `ghcWithPackages`. That function expects one argument: a
function that maps from an attribute set of Haskell packages to a list of
packages, which determines the libraries known to that particular version of
GHC. For example, the Nix expression `ghcWithPackages (pkgs: [pkgs.mtl])`
generates a copy of GHC that has the `mtl` library registered in addition to
its normal core packages:

    $ nix-shell -p "haskellPackages.ghcWithPackages (pkgs: [pkgs.mtl])"

    [nix-shell:~]$ ghc-pkg list mtl
    /nix/store/zy79...-ghc-7.10.1/lib/ghc-7.10.1/package.conf.d:
        mtl-2.2.1

This function allows users to define their own development environment by means
of an override. After adding the following snippet to `~/.nixpkgs/config.nix`,

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

it's possible to install that compiler with `nix-env -f "<nixpkgs>" -iA
myHaskellEnv`. If you'd like to switch that development environment to a
different version of GHC, just replace the `ghc7101` bit in the previous
definition with the appropriate name. Of course, it's also possible to define
any number of these development environments! (You can't install two of them
into the same profile at the same time, though, because that would result in
file conflicts.)

The generated `ghc` program is a wrapper script that re-directs the real
GHC executable to use a new `lib` directory --- one that we specifically
constructed to contain all those packages the user requested:

    $ cat $(type -p ghc)
    #! /nix/store/xlxj...-bash-4.3-p33/bin/bash -e
    export NIX_GHC=/nix/store/19sm...-ghc-7.10.1/bin/ghc
    export NIX_GHCPKG=/nix/store/19sm...-ghc-7.10.1/bin/ghc-pkg
    export NIX_GHC_DOCDIR=/nix/store/19sm...-ghc-7.10.1/share/doc/ghc/html
    export NIX_GHC_LIBDIR=/nix/store/19sm...-ghc-7.10.1/lib/ghc-7.10.1
    exec /nix/store/j50p...-ghc-7.10.1/bin/ghc "-B$NIX_GHC_LIBDIR" "$@"

The variables `$NIX_GHC`, `$NIX_GHCPKG`, etc. point to the *new* store path
`ghcWithPackages` constructed specifically for this environment. The last line
of the wrapper script then executes the real `ghc`, but passes the path to the
new `lib` directory using GHC's `-B` flag.

The purpose of those environment variables is to work around an impurity in the
popular [ghc-paths](http://hackage.haskell.org/package/ghc-paths) library. That
library promises to give its users access to GHC's installation paths. Only,
the library can't possible know that path when it's compiled, because the path
GHC considers its own is determined only much later, when the user configures
it through `ghcWithPackages`. So we [patched
ghc-paths](https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/ghc-paths-nix.patch)
to return the paths found in those environment variables at run-time rather
than trying to guess them at compile-time.

To make sure that mechanism works properly all the time, we recommend that you
set those variables to meaningful values in your shell environment, too, i.e.
by adding the following code to your `~/.bashrc`:

    if type >/dev/null 2>&1 -p ghc; then
      eval "$(egrep ^export "$(type -p ghc)")"
    fi

If you are certain that you'll use only one GHC environment which is located in
your user profile, then you can use the following code, too, which has the
advantage that it doesn't contain any paths from the Nix store, i.e. those
settings always remain valid even if a `nix-env -u` operation updates the GHC
environment in your profile:

    if [ -e ~/.nix-profile/bin/ghc ]; then
      export NIX_GHC="$HOME/.nix-profile/bin/ghc"
      export NIX_GHCPKG="$HOME/.nix-profile/bin/ghc-pkg"
      export NIX_GHC_DOCDIR="$HOME/.nix-profile/share/doc/ghc/html"
      export NIX_GHC_LIBDIR="$HOME/.nix-profile/lib/ghc-$($NIX_GHC --numeric-version)"
    fi

## How to create ad hoc environments for `nix-shell`

The easiest way to create an ad hoc development environment is to run
`nix-shell` with the appropriate GHC environment given on the command-line:

    nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [mtl pandoc])"

For more sophisticated use-cases, however, it's more convenient to save the
desired configuration in a file called `shell.nix` that looks like this:

    { nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:
    let
      inherit (nixpkgs) pkgs;
      ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
              monad-par mtl
            ]);
    in
    pkgs.stdenv.mkDerivation {
      name = "my-haskell-env-0";
      buildInputs = [ ghc ];
      shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
    }

Now run `nix-shell` --- or even `nix-shell --pure` --- to enter a shell
environment that has the appropriate compiler in `$PATH`. If you use `--pure`,
then add all other packages that your development environment needs into the
`buildInputs` attribute. If you'd like to switch to a different compiler
version, then pass an appropriate `compiler` argument to the expression, i.e.
`nix-shell --argstr compiler ghc784`.

If you need such an environment because you'd like to compile a Hackage package
outside of Nix --- i.e. because you're hacking on the latest version from Git
---, then the package set provides suitable nix-shell environments for you
already! Every Haskell package has an `env` attribute that provides a shell
environment suitable for compiling that particular package. If you'd like to
hack the `lens` library, for example, then you just have to check out the
source code and enter the appropriate environment:

      $ cabal get lens-4.11 && cd lens-4.11
      Downloading lens-4.11...
      Unpacking to lens-4.11/

      $ nix-shell "<nixpkgs>" -A haskellPackages.lens.env
      [nix-shell:/tmp/lens-4.11]$

At point, you can run `cabal configure`, `cabal build`, and all the other
development commands. Note that you need `cabal-install` installed in your
`$PATH` already to use it here --- the `nix-shell` environment does not provide
it.

# How to create Nix builds for your own private Haskell packages

If your own Haskell packages have build instructions for Cabal, then you can
convert those automatically into build instructions for Nix using the
`cabal2nix` utility, which you can install into your profile by running
`nix-env -i cabal2nix`.

## How to build a stand-alone project

For example, let's assume that you're working on a private project called
`foo`. To generate a Nix build expression for it, change into the project's
top-level directory and run the command:

    $ cabal2nix . >foo.nix

Then write the following snippet into a file called `default.nix`:

    { nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:
    nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./foo.nix { }

Finally, store the following code in a file called `shell.nix`:

    { nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:
    (import ./default.nix { inherit nixpkgs compiler; }).env

At this point, you can run `nix-build` to have Nix compile your project and
install it into a Nix store path. The local directory will contain a symlink
called `result` after `nix-build` returns that points into that location. Of
course, passing the flag `--argstr compiler ghc763` allows switching the build
to any version of GHC currently supported.

Furthermore, you can call `nix-shell` to enter an interactive development
environment in which you can use `cabal configure` and `cabal build` to develop
your code. That environment will automatically contain a proper GHC derivation
with all the required libraries registered as well as all the system-level
libraries your package might need.

If your package does not depend on any system-level libraries, then it's
sufficient to run

    $ nix-shell --command "cabal configure"

once to set up your build. `cabal-install` determines the absolute paths to all
resources required for the build and writes them into a config file in the
`dist/` directory. Once that's done, you can run `cabal build` and any other
command for that project even outside of the `nix-shell` environment. This
feature is particularly nice for those of us who like to edit their code with
an IDE, like Emacs' `haskell-mode`, because it's not necessary to start Emacs
inside of nix-shell just to make it find out the necessary settings for
building the project; `cabal-install` has already done that for us.

If you want to do some quick-and-dirty hacking and don't want to bother setting
up a `default.nix` and `shell.nix` file manually, then you can use the
`--shell` flag offered by `cabal2nix` to have it generate a stand-alone
`nix-shell` environment for you. With that feature, running

    $ cabal2nix --shell . >shell.nix
    $ nix-shell --command "cabal configure"

is usually enough to set up a build environment for any given Haskell package.
You can even use that generated file to run `nix-build`, too:

    $ nix-build shell.nix

## How to build projects that depend on each other

If you have multiple private Haskell packages that depend on each other, then
you'll have to register those packages in the Nixpkgs set to make them visible
for the dependency resolution performed by `callPackage`. First of all, change
into each of your projects top-level directories and generate a `default.nix`
file with `cabal2nix`:

    $ cd ~/src/foo && cabal2nix . >default.nix
    $ cd ~/src/bar && cabal2nix . >default.nix

Then edit your `~/.nixpkgs/config.nix` file to register those builds in the
default Haskell package set:

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

Once that's accomplished, `nix-env -f "<nixpkgs>" -qA haskellPackages` will
show your packages like any other package from Hackage, and you can build them

    $ nix-build "<nixpkgs>" -A haskellPackages.foo

or enter an interactive shell environment suitable for building them:

    $ nix-shell "<nixpkgs>" -A haskellPackages.bar.env

# Miscellaneous Topics

## How to build with profiling enabled

Every Haskell package set takes a function called `overrides` that you can use
to manipulate the package as much as you please. One useful application of this
feature is to replace the default `mkDerivation` function with one that enables
library profiling for all packages. To accomplish that, add configure the
following snippet in your `~/.nixpkgs/config.nix` file:

    {
      packageOverrides = super: let self = super.pkgs; in
      {
        profiledHaskellPackages = self.haskellPackages.override {
          overrides = self: super: {
            mkDerivation = args: super.mkDerivation (args // {
              enableLibraryProfiling = true;
            });
          };
        };
      };
    }

## How to override a cabal package version for a specific compiler version

Say the default version of ghc-events is 0.4.4.0 and does not work with ghc784
We can override this by

    $ mkdir .nixpkgs/haskell/ && cd .nixpkgs/haskell/
    $ cabal get ghc-events-0.4.3.0 && cd ghc-events-0.4.3.0 && cabal2nix --no-check ghc-events.cabal > default.nix

in the config.nix we then override the packages

    #overrides for every compiler versions
    myHaskellPackages = hp : hp.override {
      overrides = self: super:  with pkgs.haskell.lib; {
          ...
          };
      };
    myHaskellPackages784 = hp : hp.override {
      overrides = self: super:  with pkgs.haskell.lib; {
          ...
          ghc-events = dontCheck (pkgs.haskell.packages.ghc784.callPackage  ./haskell/ghc-events-0.4.3.0  {});
          ...
          };
      };
    haskellngPackages  = myHaskellPackages super.haskellPackages;
    haskell784Packages = myHaskellPackages784(myHaskellPackages super.haskell.packages.ghc784);

We can then use those packages to build environments which will pick up the desired version

    hs784  = haskell784Packages.ghcWithPackages (p: with p;
                [
                  ghc-events
                  ...
                ]
       );

    hs7101 = haskellngPackages.ghcWithPackages (p: with p;
                [
                  ghc-events
                  ...
                ]

as can be verified by

    nix-store -qR $(nix-instantiate '<nixpkgs>'  -A pkgs.hs784 ) | grep ghc-events
    nix-store -qR $(nix-instantiate '<nixpkgs>'  -A pkgs.hs7101) | grep ghc-events

## How to recover from GHC's infamous non-deterministic library ID bug

GHC and distributed build farms don't get along well:

    https://ghc.haskell.org/trac/ghc/ticket/4012

When you see an error like this one

    package foo-0.7.1.0 is broken due to missing package
    text-1.2.0.4-98506efb1b9ada233bb5c2b2db516d91

then you have to download and re-install `foo` and all its dependents from
scratch:

    # nix-store -q --referrers /nix/store/*-haskell-text-1.2.0.4 \
      | nix-store --repair-path --option binary-caches http://hydra.nixos.org

If you're using additional Hydra servers other than `hydra.nixos.org`, then it
might be necessary to purge the local caches that store data from those
machines to disable these binary channels for the duration of the previous
command, i.e. by running:

    rm /nix/var/nix/binary-cache-v3.sqlite
    rm /nix/var/nix/manifests/*
    rm /nix/var/nix/channel-cache/*
