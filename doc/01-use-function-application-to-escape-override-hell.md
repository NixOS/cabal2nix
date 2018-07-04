Use Function Application To Escape Override Hell
================================================

The Problem
-----------

The architecture of the `haskell.packages.*` hierarchy worked fine when Nixpkgs
shipped a handful of manually maintained package sets. The addition of LTS
Haskell, however, increased the number of active package sets by a factor of
10, and that number will grow further since there is a new LTS release coming
out every week. Our preferred mechanism to cope with that diversity is the
override: `haskellPackages` provides the foundation that other package sets
override according to their needs. As of today, this approach requires
approximately 25,000 overrides. Furthermore, we use 3 different types of
overrides (`override`, `overrideCabal`, and `overrideScope`) that have subtly
different purposes and capabilities. The complexity of this technique caused
non-trivial issues in the past that manifested in [severe memory leaks][5],
[insufficient flexibility][6], and [counter-intuitive behavior][7]. The current
approach is not going to scale forever --- particularly in terms of
maintainability. We need a better architecture to define Haskell package sets
that supports variant builds in a fashion that's simpler and thus easier to
understand and maintain.

The Situation Today
-------------------

Nixpkgs distributes over 50 Haskell package sets that differ in which packages
they contain, which versions of those packages they contain, and how those
builds are configured. For example, package sets intended for use with GHC
versions prior to 7.4.x neither build nor run regression test suites, because
Cabal introduced that feature only in later versions of the compiler. Package
sets compiling with GHC versions prior to 7.8.x don't support shared library
builds, because that feature never worked reliably until more recent versions
of the compiler. And builds running on Darwin don't generate the Haddock
documentation when compiling with GHC versions 7.6.x or earlier, because the
Haddock binary on that platform used to interact poorly with Darwin's `cpp` and
thus failed until that bug was eventually fixed in GHC 7.8.1.

This means that seemingly similar builds like `haskell.packages.ghc763.foo` and
`haskell.packages.ghc784.foo` actually differ in multiple dimensions: those two
`foo`s may be different versions of the same package; they are almost certainly
compiled with different versions of their respective dependencies; and their
configurations potentially differ, i.e both builds use different Cabal flags
and have different features enabled. In fact, a build of `haskellPackages.foo`
that's run on Linux/x86_64 most likely differs non-trivially from the exact
same build run on Darwin/x86_64.

Nix can handle this diversity, because it describes builds in a Turing-complete
functional language. Builds that exist in multiple variants are functions that
take the necessary dependencies plus a set of flags to enable/disable the
features we intend to support, and then map that information to appropriate
build instructions. Those functions live in one central place,
[`hackage-packages.nix`][1], and all our crazy package sets simply call those
functions with appropriate arguments to define exactly those build variants
that they need.

What sounds simple and obvious in theory, however, is a bit of a mess in
practice. Consider the package `hslua` as an example. `hackage-packages.nix`
defines builds for versions 0.3.13, 0.4.0, and 0.4.1. Those definitions look
all quite similar, so we'll omit the third definition for brevity:

    "hslua_0_3_13" = callPackage
      ({ mkDerivation, base, lua5_1, mtl }:
       mkDerivation {
         pname = "hslua";
         version = "0.3.13";
         sha256 = "e95774d21ac4378e799af4b8dce9068bfedd3109bf13b717d9ecb5817f86430a";
         configureFlags = [ "-fsystem-lua" ];
         libraryHaskellDepends = [ base mtl ];
         libraryPkgconfigDepends = [ lua5_1 ];
         testHaskellDepends = [ base ];
         description = "A Lua language interpreter embedding in Haskell";
         license = stdenv.lib.licenses.mit;
         hydraPlatforms = stdenv.lib.platforms.none;
       }) { inherit (pkgs) lua5_1; };

    "hslua" = callPackage
      ({ mkDerivation, base, bytestring, hspec, hspec-contrib, HUnit
       , lua5_1, QuickCheck, quickcheck-instances, text
       }:
       mkDerivation {
         pname = "hslua";
         version = "0.4.1";
         sha256 = "2df2b4f0566ef2244506f9830e0207fce3bece7c331129f69f446c722136173f";
         configureFlags = [ "-fsystem-lua" ];
         libraryHaskellDepends = [ base bytestring ];
         librarySystemDepends = [ lua5_1 ];
         testHaskellDepends = [
           base bytestring hspec hspec-contrib HUnit QuickCheck
           quickcheck-instances text
         ];
         description = "A Lua language interpreter embedding in Haskell";
         license = stdenv.lib.licenses.mit;
       }) { inherit (pkgs) lua5_1; };

Let's go through that code step by step. The basic structure of those
definitions is

    name_version = callPackage f { ... system dependencies ... };

where `f` is a function that given a set of build inputs produces a build. The
code invokes that function right away, so apparently `name_version` defines a
*build*, not a function that generates a build. The arguments passed to `f`
contain only system-level dependencies like `lua5_1`. Haskell dependencies like
`mtl` are not passed explicitly since the magic `callPackage` function can find
them automatically. Haskell attributes have a `_version` suffix only if the
attribute refers to an old version, like `hslua_0_3_13`. The latest version,
0.4.1, is simply called `hslua`. Last but not least, every Haskell build
expects a parameter `mkDerivation` --- our generic Haskell builder, which turns
those attribute sets into build instructions that Nix can execute. This
argument is also not passed explicitly but found by `callPackage`.

The `callPackage` magic relies on two advanced techniques: fixed points and
reflection. Nixpkgs defines the Haskell package set recursively as a function
`self: { ... }` that takes as an argument the attribute set it's going to
produce and then uses that information to the produce the attribute set it's
already received as an argument. It's a mind-boggling way to define a recursive
attribute set `rec { ... }` without actually using the `rec` keyword. Every
computable function of this kind has a fixed point: a value `x` that satisfies
`f x = x`. That `x` is the result we actually want --- it's an ordinary
attribute set with all the `self`-references resolved and replaced by actual
values. The details of fixed points aren't important, though. What's important
is that our package set knows its final value already while we're still
defining it! This allows for the existence of the `callPackage` function:

    self: let
            callPackage = stdenv.lib.callPackageWith self;
          in
            self // { ... Haskell package definitions here ... }

The expression `callPackage f args` translates to `stdenv.lib.callPackageWith
self f args`. [`callPackageWith`][2] then uses reflection to determine the
names of all arguments expected by `f`. It finds those names that don't exist
in `args` in `self` and adds them to `args` to complete the function call. In
other words, parameters of `f` that are not specified in `args` are
automatically filled in from `self` --- the Haskell package set. This is why
system packages are passed explicitly (`self` doesn't contain them), but
Haskell dependencies are not (`self` does contain them).

This feature is nice in package sets human beings write manually, because it
allows us to keep function calls short and concise. A computer program
generates the file `hackage-packages.nix`, though, so arguably we could have it
bind all Haskell names explicitly with `inherit (self) base mtl` just as well.
But `callPackage` has another feature that's more important than implicit name
resolution: it adds the method `override` to the generated derivation, which
can "undo" the function application to change the value of arguments that were
used originally. We can say

    hslua.override { mtl = self.mtl_2_1_3_1; }

to instantiate a variant of `hslua` that compiles with that particular version
of `mtl` rather than the default one. The same way, we can change the behavior
of `mkDerivation`! For example, the expression

    hsload.override {
      mkDerivation = args: self.mkDerivation (args // { doCheck=false; });
    }

gives us a variant of `hslua` that has the test suite disabled. These kind of
overrides are so useful that we've defined a whole [set of helper functions][3]
that use this technique to customize the default definitions from
`hackage-packages.nix` in any way we need.
to define
Now, Haskell packages sets are just collections of overrides that change the
default choices made in `hackage-packages.nix` as required. For example, the
package set for [LTS Haskell version 2.8][4] specifies:

    "hslua" = doDistribute super."hslua_0_3_13";

It renames `hslua_0_3_13` to `hslua` to ensure that all Haskell builds in that
package set use the older version instead of the latest one. Furthermore, we
would like Hydra builds of `haskell.packages.lts-2_8.hslua` enabled. The
original definition from `hackage-packages.nix` disabled those builds via

    hydraPlatforms = stdenv.lib.platforms.none

which makes sense in `haskellPackages`, because it prefers the latest version
of the package, but the older LTS set isn't happy with that choice, so it uses
a `doDistribute` override to replace the original empty set of `hydraPlatforms`
with something more appropriate.

Generally speaking, all of our 50+ Haskell package sets are essentially just
variants of `hackage-packages.nix` with certain choices overridden if
necessary.

Possible Improvements
---------------------

It's unclear how to improve upon the current architecture.

The override approach produces a weird structure: first, we define a build
function for every Haskell package --- like one would expect in a purely
functional language ---, but then that function isn't exposed. Instead, we call
the function right away to create a derivation. However, we can't use an honest
function call, because we really need a *function* later, so we're forced to
use freaky lazy-evaluation features that allow us to undo the function call
with `override` later to make the derivation look like the function again that
it probably should have been in the first place.

One possible approach is to take `callPackage` out of `hackage-packages.nix`.
Instead of derivations, `hackage-packages.nix` could provide functions that
package sets then call with appropriate parameters to define the actual build.
This would require some 10,000 functions plus another ~7,000 function
applications per package set. In the current architecture, some attributes from
`hackage-packages.nix` are passed through unmodified, whereas the new approach
requires each package set to define every build explicitly. It's unclear how
that would affect the Nix interpreter in terms of memory requirements and
performance.

Furthermore, just taking `callPackage` out of `hackage-packages.nix` won't
remedy the need for overriding, because our definitions don't mention any of
the build configuration flags, like `doCheck`. It's not possible to call the
function

    "hslua" =
       { mkDerivation, base, bytestring, [... more inputs omitted ...] }:

       mkDerivation {
         pname = "hslua";
         version = "0.4.1";
         [... more attributes omitted ...]
       };

with `hslua { doCheck = false; ... }` because `doCheck` is an argument of
`mkDerivation`, which is hidden inside of that function. The established
approach to solve that problem is to wrap `mkDerivation`, i.e.

    hslua {
      mkDerivation = args: self.mkDerivation (args // { doCheck=false; });
    }

but this feels cumbersome compared to straight function application. We can
lift all those `mkDerivation` arguments into the outer function:

    "hslua" =
       { mkDerivation, base, bytestring, [... more inputs omitted ...]
       , doCheck ? true, doHaddock ? true, enableSharedLibraries ? true
       }:
       mkDerivation {
         pname = "hslua";
         version = "0.4.1";
         inherit doCheck doHaddock;
         [... more attributes omitted ...]
       };

but this adds a dozen highly redundant arguments to every single definition!
There might be clever ways around mentioning each of them explicitly, like:

    "hslua" =
       { mkDerivation, base, bytestring, [... more inputs omitted ...]
       , ...
       } @ args:
       stdenv.lib.callPackageWith args mkDerivation {
         pname = "hslua";
         version = "0.4.1";
         [... more attributes omitted ...]
       };

Yet that is exactly the kind of stuff we wanted to avoid in the first place. So
maybe it would be best to batch all those variant options in an attribute set
of their own?

    "hslua" =
       { mkDerivation, base, bytestring, [... more inputs omitted ...]
       , config ? {}
       }:
       mkDerivation ({
         pname = "hslua";
         version = "0.4.1";
         [... more attributes omitted ...]
       } // config);

This approach allows us to call `hslua { config.doCheck = false; }`, which
seems nice enough. At the same time, it allows us to call

    hslua { config.pname = "this-is-not-hslua"; }

as well, which feels like a bug.

An entirely different approach is to take advantage of lazy evaluation to
define all possible build variants at once:

    let
       showFlag = str: b: (if b then "with" else "without") + "-" + str;

       mkDerivation = { pname, version, buildDepends ? []
                      , doCheck ? true
                      } @ args:
                      {
                        name = pname + "-" + version + "-" + showFlag "check" doCheck;

                        enableChecking = mkDerivation (args // { doCheck = true; });
                        disableChecking = mkDerivation (args // { doCheck = false; });
                      };

       "hslua" = { base ? null }:
                 mkDerivation {
                   pname = "hslua";
                   version = "0.4.1";
                   buildDepends = [ base ];
                 };
    in
      [ ((hslua { base = "base"; }).name)
        ((hslua { base = "base"; }).disableChecking.name)
        ((hslua { base = "base"; }).disableChecking.disableChecking.enableChecking.name)
      ]

Each of those styles has its own advantages and disadvantages which aren't
fully understood yet. Hopefully, this summary provides some food for thought
and helps kick off a fruitful discussion.



[1]: https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/hackage-packages.nix
[2]: https://github.com/NixOS/nixpkgs/blob/master/lib/customisation.nix#L72-L97
[3]: https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/lib.nix
[4]: https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/configuration-lts-2.8.nix#L4171
[5]: https://github.com/NixOS/nixpkgs/issues/6192
[6]: https://github.com/NixOS/nixpkgs/issues/7953
[7]: https://github.com/NixOS/nixpkgs/issues/9871
