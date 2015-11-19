Publish All of Hackage
======================

The Problem
-----------

Users of Nixpkgs want to build Haskell packages that have no recent release,
i.e. the software's authors have not updated their code to deal with modern
versions of the respective build dependencies. The required old versions,
however, may not be available in Nixpkgs, and then the build cannot succeed. At
that point, we recommend our users to
[add the missing package versions themselves][1] through the override mechanism
in `~/.nixpkgs/config.nix`. That approach works fine, but it's challenging for
new users and it's certainly inconvenient.

The Situation Today
-------------------

We publish 11,216 package versions out of 62,354 available ones, i.e. 18% of
Hackage. We select packages for distribution using the following criteria:

1. the latest version of every package (9,058 builds),

2. old versions required by LTS Haskell (2,137 builds), and

3. old versions manually configured in the `extra-packages` field of
   [`configuration-hackage2nix.yaml`][2] (21 builds).

The resulting [`hackage-packages.nix`][3] file is ~8.3 MByte large.

Now, we could simply publish all of Hackage, i.e. all 62,354 available builds.
That would certainly make the lives of many Haskell hackers easier, but
unfortunately the resulting `hackage-packages.nix` file would require 52 MByte
of space. Furthermore, simple benchmarks suggest that `nix-instantiate` needs a
whopping 4 seconds to instantiate a `ghcWithPackages` environment from the new
database where instantiating the same environment used to take 0.45 seconds
before --- a performance drop by a factor of 8.

Possible Improvements
---------------------

We could continue to publish the "small" `hackage-packages.nix` database by
default, but generate an additional large database that we'd store in a
separate location. We would then add support for an option to
`~/.nixpkgs/config.nix` users could set to opt in to using the large database
if they want to. That way, Haskell users would have an option to get all of
Hackage without causing a performance drop for everyone else.

One issue is that checking that ~50MB file into Nixpkgs might be a bad idea,
because it sets a dangerous precedent. Arguably, if we check all of Hackage
into Nixpkgs, then we cannot reasonably say *no* to someone who wants to check
all of CPAN into Nixpkgs too, and before we know it our Git repository triples
in size. So it might be wise to put the full Hackage variant of
`hackage-packages.nix` into a separate repository that we fetch via
`builtins.fetchurl` if the corresponding option is enabled.

An altogether different approach is to improve our tools like `cabal2nix` to
make it convenient to add old versions from Hackage into a local Nixpkgs
instance, i.e. by extending the program to generate builds for all missing
versions to complete the transitive closure of the requested build so that
users don't have to run it a dozen times because multiple old versions are
missing. Conceivably, we could also provide tools that manage
`~/.nixpkgs/config.nix`, which might be a good idea independent of these
Haskell-specific issues.


[1]: http://nixos.org/nixpkgs/manual/#how-to-create-nix-builds-for-your-own-private-haskell-packages
[2]: https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/configuration-hackage2nix.yaml
[3]: https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/hackage-packages.nix
