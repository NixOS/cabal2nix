Cabal2nix
=========

[![hackage release](https://img.shields.io/hackage/v/cabal2nix.svg?label=hackage)](http://hackage.haskell.org/package/cabal2nix)
[![stackage LTS package](http://stackage.org/package/cabal2nix/badge/lts)](http://stackage.org/lts/package/cabal2nix)
[![stackage Nightly package](http://stackage.org/package/cabal2nix/badge/nightly)](http://stackage.org/nightly/package/cabal2nix)
[![travis build status](https://img.shields.io/travis/NixOS/cabal2nix/master.svg?label=travis+build)](https://travis-ci.org/NixOS/cabal2nix)

`cabal2nix` converts a single Cabal file into a single Nix build expression.
For example:

    $ cabal2nix cabal://mtl
    { mkDerivation, base, lib, transformers }:
    mkDerivation {
      pname = "mtl";
      version = "2.2.1";
      sha256 = "1icdbj2rshzn0m1zz5wa7v3xvkf6qw811p4s7jgqwvx1ydwrvrfa";
      libraryHaskellDepends = [ base transformers ];
      homepage = "http://github.com/ekmett/mtl";
      description = "Monad classes, using functional dependencies";
      license = lib.licenses.bsd3;
    }

Cabal files can be referred to using the magic URL `cabal://NAME-VERSION`,
which will automatically download the file from Hackage. Alternatively, a
direct `http://host/path/pkg.cabal` URL can be provided, as well as a
`file:///local/path/pkg.cabal` URI that doesn't depend on network access.
However, if the source hash is not already in `cabal2nix`'s cache or provided
using the `--sha256` option, `cabal2nix` still needs to download the source
code to compute the hash, which still causes network traffic. Run the utility
with `--help` to see the complete list of supported command-line flags.

Detailed instructions on how to use those generated files with Nix can be found at
https://haskell4nix.readthedocs.io/nixpkgs-users-guide.html#how-to-create-nix-builds-for-your-own-private-haskell-packages.

`cabal2nix` can also build derivations for projects from other sources than
Hackage. You only need to provide a URI that points to a cabal project. The
most common use-case for this is probably to generate a derivation for a
project on the local file system:

    $ cabal get mtl-2.2.1 && cd mtl-2.2.1
    $ cabal2nix .
    { mkDerivation, base, lib, transformers }:
    mkDerivation {
      pname = "mtl";
      version = "2.2.1";
      src = ./.;
      libraryHaskellDepends = [ base transformers ];
      homepage = "http://github.com/ekmett/mtl";
      description = "Monad classes, using functional dependencies";
      license = lib.licenses.bsd3;
    }

This derivation will not fetch from hackage, but instead use the directory which
contains the derivation as the source repository.

`cabal2nix` currently supports the following repository types:

* directory
* source archive (zip, tar.gz, ...) from http or https URL or local file.
* git, mercurial, svn or bazaar repository

## `hackage2nix`

This repository also contains, in the [`hackage2nix/`](./hackage2nix) directory,
the tool to update the Haskell packages in
[nixpkgs](https://github.com/NixOS/nixpkgs). It has its own README there.

## Building

For ease of contribution, we support these methods to build `cabal2nix`:

* `stack build` if you have [_Stack_](http://haskellstack.org/)
  * The Stack build can use system dependencies from a pinned nixpkgs
    version for increased reproducibility (see [`stack.yaml`](./stack.yaml)).
    On NixOS this happens automatically.
    On non-NixOS, use `stack --nix build` for that.
* `cabal install` if you have [_cabal-install_](https://www.haskell.org/cabal/)
  installed
* TODO: Add a build method that requires only `nix` preinstalled,
  also pins nixpkgs, and is thus fully reproducible.

When making changes, please ensure that all these methods continue to work.
