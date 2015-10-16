The Future of Haskell Packaging in Nix
======================================

Fellow Nixers and Haskellers,

the [Haskell NG][1] effort allowed us for the first time to distribute the
latest version of every package known to Hackage --- well over 8,000 builds in
total ---, and this seemed like a crazy leap forward at the time. These days, we
distribute [60+ Haskell package sets][2] that represent different views into
Hackage built with 8+ different compilers, and there's probably a lot more
variety out there, living in our user's `config.nix` files, that we don't even
know about. The requirements on our Haskell infrastructure become ever more
complex. New questions pop up due to the growing popularity of Stack; our
support for old compiler versions is somewhat lacking; and we still don't have
convincing answers to questions like: how do I compile my package with
profiling support?

The issues we have to address are mostly known and understood. What we lack are
brilliant ideas how to solve them.

This thread is an attempt to make some headway towards that end through open
discussion and collaboration. Instead of everyone having to browse a couple of
thousand Github tickets to find out what's going on, I'll regularly pick one
particular aspect that needs improvement and post a short article outlining the
issues we have and potential solutions that we know of so that constructive
dialogue can ensue.

Everyone is invited to participate in the conversation with ideas, questions,
suggestions, or references to Github Pull Requests that implement a perfect,
production-ready solution waiting to be merged.

The issues we've identified so far are:

- [Use Function Application To Escape Override Hell](01-use-function-application-to-escape-override-hell.md)
- [Guarantee Consistent Builds and Obsolete `overrideScope`](02-guarantee-consistent-builds-and-obsolete-overridescope.md)
- [Map Cabal Files To Nix Without Information Loss](03-map-cabal-files-to-nix-without-information-loss.md)

[1]: https://github.com/NixOS/nixpkgs/commit/c0c82ea2ebbcf0632260a931cf832cac1c8a014e
[2]: https://github.com/NixOS/nixpkgs/blob/master/pkgs/top-level/haskell-packages.nix
