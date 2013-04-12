How to maintain Haskell Packages in Nix
=======================================

## Overview over the tool-chain

There are two utilities, `cabal2nix` and `hackage4nix`, that automate
maintenance to a large extend. We intend to merge them into one program,
eventually, but the necessary re-factoring hasn't been done yet since
this is not a high priority.

### Cabal2nix

`cabal2nix` converts a single Cabal file into a single Nix build
expression. For example:

    $ cabal2nix cabal://yesod-0.9.1
    { cabal, attoparsecText, blazeBuilder, blazeHtml, hamlet, httpTypes
    , monadControl, parsec, shakespeareCss, shakespeareJs, text, time
    , transformers, unixCompat, wai, waiExtra, warp, yesodAuth
    , yesodCore, yesodForm, yesodJson, yesodPersistent
    }:

    cabal.mkDerivation (self: {
      pname = "yesod";
      version = "0.9.1";
      sha256 = "1ag3lca75lrriycbscspb5yyishacgxjx0rybc3x4z1dqnkn1r71";
      isLibrary = true;
      isExecutable = true;
      buildDepends = [
        attoparsecText blazeBuilder blazeHtml hamlet httpTypes monadControl
        parsec shakespeareCss shakespeareJs text time transformers
        unixCompat wai waiExtra warp yesodAuth yesodCore yesodForm
        yesodJson yesodPersistent
      ];
      meta = {
        homepage = "http://www.yesodweb.com/";
        description = "Creation of type-safe, RESTful web applications";
        license = self.stdenv.lib.licenses.bsd3;
        platforms = self.ghc.meta.platforms;
      };
    })

Cabal files can be referred to using the magic URL
`cabal://NAME-VERSION`, which will automatically download the file from
Hackage. Alternatively, a direct `http://host/path/pkg.cabal` URL can be
provided, as well as a `file:///local/path/pkg.cabal` URI that doesn't
depend on network access. Run the utility with `--help` to see the
complete list of supported command line flags.

To add a new package to Nix, the following steps need to be performed:

    $ cd $NIXPKGS_ALL/pkgs/development/libraries/haskell
    $ mkdir foo
    $ cabal2nix cabal://foo-1.0 >foo/default.nix

Then add an appropriate attribute to
`pkgs/top-level/haskell-packages.nix`, for example:

    foo = callPackage ../development/libraries/haskell/foo {};

### Hackage4nix

The `hackage4nix` utility re-generates *all* expressions found in the
`nixpkgs` database in place. This is useful to ensure that all packages
have been generated with a recent version of the tool-chain.
Furthermore, `hackage4nix` adds default settings for the
`meta.maintainers` and `meta.platforms` attribute if these aren't
configured yet. Generally speaking, running

    $ hackage4nix $NIXPKGS_ALL

should be a *no-op* --- i.e. no files should change! If there are
changes, these indicate that a file has been modified manually, and then
these changes must be investigated to find out what is going on.

Last but not least, `hackage4nix` generates a list of all updates
available from Hackage. (Run `cabal update` to make sure that your local
copy of the Hackage database is up-to-date!) For example:

    $ hackage4nix $NIXPKGS_ALL
    The following updates are available:

    WebBits-Html-1.0.1:
      cabal2nix cabal://WebBits-Html-1.0.2 >$NIXPKGS_ALL/pkgs/development/libraries/haskell/WebBits-Html/default.nix

    happstack-server-6.1.6:
      cabal2nix cabal://happstack-server-6.2.1 >$NIXPKGS_ALL/pkgs/development/libraries/haskell/happstack/happstack-server.nix
      cabal2nix cabal://happstack-server-6.2.2 >$NIXPKGS_ALL/pkgs/development/libraries/haskell/happstack/happstack-server.nix

    primitive-0.3.1:
      cabal2nix cabal://primitive-0.4 >$NIXPKGS_ALL/pkgs/development/libraries/haskell/primitive/default.nix

    repa-2.1.1.5:
      cabal2nix cabal://repa-2.1.1.6 >$NIXPKGS_ALL/pkgs/development/libraries/haskell/repa/default.nix

    unix-compat-0.2.2.1:
      cabal2nix cabal://unix-compat-0.3 >$NIXPKGS_ALL/pkgs/development/libraries/haskell/unix-compat/default.nix

    vector-0.7.1:
      cabal2nix cabal://vector-0.8 >$NIXPKGS_ALL/pkgs/development/libraries/haskell/vector/default.nix

These updates can be performed automatically by running the `cabal2nix`
command given by `hackage4nix`. If there is more than one possible
update, then all of them will be shown. Note, however, that some updates
break compilation of other packages, because they depend on very
specific versions of their build inputs, so please be careful when
performing updates!

## Current State of Affairs

The tool-chain is stable. As of today, 2013-04-12, virtually all Haskell
packages available in Nix have been generated automatically from their
Cabal files. There are only a handful of exceptions, which we cannot
generate because these packages aren't available on Hackage. The list of
those packages is hard-coded into the
[`hackage4nix.hs`](http://github.com/NixOS/cabal2nix/blob/master/src/Hackage4Nix.hs)
binary in the function `badPackagePaths`.

Furthermore, Hackage4Nix will not re-generate packages that have been
patched, i.e. that define any of the following attributes:

        (pre|post)Configure
        (pre|post)Install
        patchPhase
        patches

These packages are considered for updates, however.

The complete list of Haskell packages available in Nix is generated by
the tool [`package-list`](http://github.com/peti/package-list),
and published at <http://cryp.to/haskell-in-nixpkgs.txt>. Hackage picks
it up from there and generates links on each package's homepage to the
corresponding page in Hydra automatically. See [ticket
875](http://hackage.haskell.org/trac/hackage/ticket/875) for further
details.
