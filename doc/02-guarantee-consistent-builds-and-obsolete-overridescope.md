Guarantee Consistent Builds and Obsolete `overrideScope`
========================================================

The Problem
-----------

A lot of effort goes into curated package sets like Stackage, but even so we
can compile only ~50% of the packages available from Hackage. It appears to be
the nature of the game: when `lens` 2.x comes out with a fundamentally new API,
then some packages will adopt the new version and others won't. A consistent
package set must chose on which side of the fence it wants to live. Either
those packages that depend on `lens` 1.x compile or those that depend on 2.x
compile --- but not both.

Now, Nix is not limited to one particular version of `lens` --- we can have
both versions available at the same time. But it's difficult to take advantage
of that feature, because once you start mixing `lens` 1.x and 2.x in the same
package set, you risk inconsistent builds, i.e. builds where some part of the
dependency tree refers to `lens` 1.x and another part refers to `lens` 2.x.
It's a bad idea to try and link those two trees together into one executable;
we are fortunate that `Cabal` detects this error during the configure phase and
aborts the build!

We need a mechanism that can mix multiple package versions within a package
set, but that also guarantees consistency for every single build. We hoped
`overrideScope` would be that mechanism, but somehow it hasn't quite lived up
to the promise, mostly because it is hard to understand.

The Situation Today
-------------------

Consider an executable package `foobar` that depends on the libraries `foo` and
`bar`, each of which depends on `lens`. The corresponding definitions in
`hackage-packages.nix` --- stripped down to the relevant bits --- look as
follows:

    "lens"     = ... lens version 1.x ...;
    "lens_2_0" = ... lens version 2.x ...;

    "foo" = callPackage
        ({ mkDerivation, lens }:
        mkDerivation {
          pname = "foo";
          libraryHaskellDepends = [lens];
        }) {};

    "bar" = callPackage
        ({ mkDerivation, lens }:
        mkDerivation {
          pname = "bar";
          libraryHaskellDepends = [lens];
        }) {};

    "foobar" = callPackage
        ({ mkDerivation, lens }:
        mkDerivation {
          pname = "foobar"; [...]
          libraryHaskellDepends = [foo bar];
        }) {};

Let's assume that `foo` won't compile in that setup because it requires `lens`
version 2.x. We can remedy that by adding an override to
`configuration-common.nix` that says:

    foo = super.foo.override { lens = self.lens_2_0; };

That change fixes the build of `foo`, but `foobar` remains broken, because now
it pulls in both `lens` 1.x and 2.x simultaneously through its dependencies. If
`bar` works only with `lens` 1.x, then there is nothing we can do: the version
constraints conflict and we cannot compile `foobar`. If `bar` *does* support
`lens` 2.x, however, then we can just switch it to the newer version with:

    bar = super.bar.override { lens = self.lens_2_0; };

Now we can compile `foobar`! Unfortunately, that change may break other builds.
There is a reason why `lens` 1.x is our default choice. If any other package
depends on `bar` as well as `lens` 1.x (directly or indirectly), then it will
no longer compile after that change.

We can avoid that side-effect by localizing the override to `foobar`:

    foobar = super.foobar.override {
      bar = self.bar.override { lens = self.lens_2_0; };
    };

That approach allows us to compile `foobar`, while still leaving the default
version of `bar` at `lens` 1.x, like most of our packages require. Overriding
build inputs this way works fine, and we have used this technique for many
years to fix builds that require non-default versions to compile. The downside
of these nested overrides is that the tend to become freaky complicated if a
package needs overriding that is sufficiently deep in the dependency tree. The
GHC 7.8.4 package set, for example, needed many such overrides because its
default version of `mtl` was stuck at version 2.1.x all the while large parts
of Hackage had moved on to `mtl` 2.2.x. Since `mtl` is a rather fundamental
package, we had nested overrides 3-4 levels deep that were highly repetitious,
too. It was a mess.

Haskell NG improved on that situation by adding `overrideScope`. That function
changes the package set ("scope") in which Nix evaluates a build expression.
The override

    foobar = super.foobar.overrideScope (self: super: { lens = self.lens_2_0; });

creates a new temporary package set, replaces `lens` with `lens_2_0` in it, and
then evaluates `foobar`. The `callPackage` function picks up the re-written
`lens` attribute, which means that there's no need to override that choice
explicitly in all dependencies of `foobar`. One could say that `overrideScope`
implements "deep overriding", i.e. it applies an override to the given
derivation as well as all sub-derivations that it refers to.

Unfortunately, we lack a proper understanding of how expensive that technique
is memory and performance-wise. In the past, we've occasionally crashed Nix
with this kind of stuff --- keep in mind that the interpreter creates a whole
new package set for every build that uses this mechanism ---, but when used
sparingly, `overrideScope` seems to work okay.

In some cases, `overrideScope` won't work at all, i.e. when confronted with
builds that have explicitly passed arguments. For example, let's say that
`lens` 3.x comes. So we try to compile `foobar` like this:

    foobar = super.foobar.overrideScope (self: super: { lens = self.lens_3_0; });

That build will fail, because we added an explicit override for `foo` earlier
that committed the build to `lens_2_0`, and `overrideScope` will not affect
that choice since that build input is not picked up with `callPackage`. So
`foobar` will pull in both `lens` 2.x and 3.x despite the use of
`overrideScope`.


Possible Improvements
---------------------

We generate Haskell build expressions automatically with `cabal2nix`, and that
tool knows the complete dependency tree for every package. So it would be
possible to generate builds that expect as function arguments not just their
immediate dependencies but the transitive closure of all dependencies. Build
expressions would then call their direct dependencies, passing in appropriate
versions of their respective dependencies, etc. For example:

    "foobar" = callPackage
        ({ mkDerivation, foo, bar, many, other, inputs, of, lens }:
        let lens' = lens.override { inherit many other inputs of lens; };
            foo'  = foo.override { lens = lens'; };
            bar'  = bar.override { lens = lens'; };
        in
        mkDerivation {
          pname = "foobar"; [...]
          libraryHaskellDepends = [foo' bar'];
        }) {};

Now, `foobar` expects every single package that occurs anywhere inside of its
dependency tree as an argument, and it constructs the dependency tree using
those arguments. So the build must be consistent. It's impossible for `foobar`
to refer to two incompatible versions of `lens`, because its inputs always use
the same version.

Consequently,

    foobar.override { mtl = self.mtl_2.4.0; }

gives us is a version of `foobar` that has its entire dependency tree built
with `mtl` 2.4.x. We could even get rid of the override altogether if we adopt
the suggestions from "Use Function Application To Escape Override Hell" and
remove `callPackage` from `hackage-packages.nix`. We'd define all builds as
straight functions

    "foobar" =
        { mkDerivation, foo, bar, many, other, inputs, of, lens }:
        let lens' = lens { inherit many other inputs of lens; };
            foo'  = foo { lens = lens'; };
            bar'  = bar { lens = lens'; };
        in
        mkDerivation {
          pname = "foobar"; [...]
          libraryHaskellDepends = [foo' bar'];
        };

and invoke them from inside of a package set with:

    callPackage foobar { mtl = self.mtl_2.4.0; }

This would give us a guarantee for consistent builds without any overrides.
