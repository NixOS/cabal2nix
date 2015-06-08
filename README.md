Cabal2nix
=========

`cabal2nix` converts a single Cabal file into a single Nix build expression.
For example:

    $ cabal2nix cabal://mtl
    { mkDerivation, base, stdenv, transformers }:
    mkDerivation {
      pname = "mtl";
      version = "2.2.1";
      sha256 = "1icdbj2rshzn0m1zz5wa7v3xvkf6qw811p4s7jgqwvx1ydwrvrfa";
      buildDepends = [ base transformers ];
      homepage = "http://github.com/ekmett/mtl";
      description = "Monad classes, using functional dependencies";
      license = stdenv.lib.licenses.bsd3;
    }


Cabal files can be referred to using the magic URL `cabal://NAME-VERSION`,
which will automatically download the file from Hackage. Alternatively, a
direct `http://host/path/pkg.cabal` URL can be provided, as well as a
`file:///local/path/pkg.cabal` URI that doesn't depend on network access.
However, if the source hash is not already in `cabal2nix`'s cache or provided
using the `--sha256` option, `cabal2nix` still needs to download the source
code to compute the hash, which obviously still causes network traffic. Run the
utility with `--help` to see the complete list of supported command line flags.

Detailed instructions how to use those generated files with Nix can be found at
https://nixos.org/wiki/Haskell.

`cabal2nix` can also build derivations for projects from other sources than
hackage. You only need to provide an URI that points to a cabal project. The
most common usecase for this is probably to generate a derivation for a project
on the local file system:

    $ cabal get mtl-2.2.1 && cd mtl-2.2.1
    $ cabal2nix .
    { mkDerivation, base, stdenv, transformers }:
    mkDerivation {
      pname = "mtl";
      version = "2.2.1";
      src = ./.;
      buildDepends = [ base transformers ];
      homepage = "http://github.com/ekmett/mtl";
      description = "Monad classes, using functional dependencies";
      license = stdenv.lib.licenses.bsd3;
    }

This derivation will not fetch from hackage, but instead use the directory which
contains the derivation as the source repository.

`cabal2nix` currently supports the following respository types:

* directory
* source archive (zip, tar.gz, ...) from http or https URL or local file.
* git, mercurial, svn or bazaar repository
