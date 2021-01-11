Map Cabal Files To Nix Without Information Loss
===============================================

The Problem
-----------

Our implementation of `cabal2nix` loses important information, because its
mapping from Cabal to Nix is too simplistic. Cabal supports conditional values
that depend on

- the OS and architecture on which the build takes place,
- the compiler version,
- the respective versions of the build dependencies, and
- Cabal flags.

But we don't capture those conditional settings in the generate Nix file.
Instead, we resolve those conditions with a set of hard-coded assumptions that
work well for Linux/x86_64 using GHC 7.10.2, but other build environments often
fail to compile the expression because some essential information is missing.


The Situation Today
-------------------

Consider the Cabal file for [`conduit-1.2.5`][2]. Among other things, it says:

    Library
      Build-depends:       base              >= 4.3   && < 5
                         , resourcet         >= 1.1   && < 1.2
                         , exceptions        >= 0.6
                         , lifted-base       >= 0.1
                         , transformers-base >= 0.4.1 && < 0.5
                         , transformers      >= 0.2.2 && < 0.5
                         , mtl
                         , mmorph
      if !impl(ghc>=7.9)
        build-depends:   void                >= 0.5.5

The library depends on the module `Data.Void`, which GHC's `base` library
contains starting with version 7.9.x, but older versions of GHC don't have that
module, so the build needs an additional 3rd-party library, `void`, to provide
that code. The Nix build expression for `conduit`, however, does not capture
that information. `cabal2nix` omits `void` from the list of dependencies
entirely:

    libraryHaskellDepends = [
      base exceptions lifted-base mmorph mtl resourcet transformers
      transformers-base
    ];

The [conversion function][1] used by `cabal2nix` doesn't translate the
conditional from Cabal into Nix. Instead, the function
[resolves all conditionals][3], assuming that

- the build will take place with the same architecture, OS, and compiler
version as were used to compile `cabal2nix`,

- all Haskell dependencies are available in all versions, and

- all Cabal flags have their default value unless specified otherwise in a
[hard-coded list][4].

For the majority of packages, that heuristic produces build expressions that
work fine, but In case of `conduit` the result won't compile with compilers
prior to GHC 7.10.x, and thus a
[manually configured override is necessary to fix the build][5].

The lack of support for conditionals is particularly annoying when it comes to
packages that make extensive use of Cabal flags, like `git-annex`. There are
numerous flags that determine the feature set supported by the build, i.e.
whether to include the "assistant" or not. Now, Stackage package sets like LTS
Haskell do contain `git-annex`, but they don't contain all the dependencies
that are necessary to build the assistant. Consequently, `git-annex` ought to
compile with `-f-assistant` within a context of Stackage. At the same time, we
want to provide the assistant feature to Nixpkgs users --- and we can, because
we have all of Hackage, not just Stackage!

So what we do is this: our generated build expression for `git-annex` by
default includes the assistant, and that is the build `gitAndTools.git-annex`
refers to. Then we define [variants of that build without the assistant][5],
and these are the ones `haskell.packages.lts-x_y.git-annex` refers to. Now, we
define that variant by setting the Cabal flag `-f-assistant`, but that override
does not change the fact that all those dependencies needed only to build the
assistant are still listed as build inputs in the Nix expression for
`git-annex`! Cabal won't use these dependencies during the build, but Nix will
create them and include them into the build environment nonetheless, which
defeats the purpose of having that flag in the first place.

Possible Improvements
---------------------

Nix expressions must capture the complete logic that the underlying Cabal file
specifies. In case of the `conduit` example, the generated code should say:

    libraryHaskellDepends = [
      base exceptions lifted-base mmorph mtl resourcet transformers
      transformers-base
    ] ++ lib.optional (lib.versionOlder ghc.version "7.9") void;

Furthermore, Cabal flags should be mapped to boolean function arguments that
enable/disable the underlying feature and thereby modify how the Nix expression
evaluates, so that no extraneous build inputs occur.


[1]: https://github.com/NixOS/cabal2nix/blob/master/distribution-nixpkgs/src/Distribution/Nixpkgs/Haskell/FromCabal.hs#L36
[2]: http://hackage.haskell.org/package/conduit-1.2.5/conduit.cabal
[3]: http://hackage.haskell.org/package/Cabal-1.22.4.0/docs/Distribution-PackageDescription-Configuration.html#v:finalizePackageDescription
[4]: https://github.com/NixOS/cabal2nix/blob/master/distribution-nixpkgs/src/Distribution/Nixpkgs/Haskell/FromCabal/Flags.hs
[5]: https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/configuration-ghc-7.8.x.nix#L142
[5]: https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/configuration-common.nix#L66-L80
