# `hackage2nix`

This tool is used to update the Haskell package set in
[nixpkgs](https://github.com/NixOS/nixpkgs)
([`hackage-packages.nix`](https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/hackage-packages.nix)).

Most of the time we do not need run this manually (since it is run automatically
on the upstream nixpkgs).  Exceptions:

* To update the Haskell package set in nixpkgs.

* To test that a structural change to nixpkgs (e.g. name change of a system
  package) does not break Haskell packages.

* To test changes to `hackage2nix` itself.

#### Video tutorial

There is a [video on YouTube](https://www.youtube.com/watch?v=qX0mgtSm360)
that gives a thorough walk-through of these steps.

It also explains many of the background considerations.

#### Requirements

This must be run on a **case sensitive file system**.
On OSX you can use [this](https://gist.github.com/dixson3/8360571) to create one
(you can replace `60g` with `4g` as that should be plenty).

#### Setup

This shows the setup that nixpkgs Haskell maintainers use to iterate on both
`cabal2nix`, `hackage2nix`, and the Haskell package set in nixpkgs.

It clones `cabal2nix` and puts a `nixpkgs` checkout into it, which the
repo's `update-nixpkgs.sh` script will update for you.

_Replace `git@github.com:NixOS/nixpkgs.git` with a fork you have push access
to._

```sh
git clone https://github.com/NixOS/cabal2nix.git
cd cabal2nix
git clone git@github.com:NixOS/nixpkgs.git # replace this with your fork
git clone https://github.com/commercialhaskell/all-cabal-hashes.git --branch hackage hackage
cabal install
```

#### Running `hackage2nix`

Runs hackage2nix and pushes a commit with the updated `hackage-packages.nix`
file to your fork of `nixpkgs`.

```sh
./update-nixpkgs.sh
```
