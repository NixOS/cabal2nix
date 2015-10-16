Set up a Sufficiently Powerful Build Farm
=========================================

The Problem
-----------

`hydra.nixos.org` does not compile any of our LTS Haskell package sets. This
means that users of `haskell.packages.lts-x_y` cannot get any pre-compiled
binaries. It also means that those builds aren't verified, i.e. we won't notice
when changes to Nixpkgs break builds in those package sets. Furthermore, we
have [no pre-compiled binaries with profiling support][1].

The Situation Today
-------------------



Possible Improvements
---------------------

[1]: https://github.com/NixOS/nixpkgs/issues/10143
