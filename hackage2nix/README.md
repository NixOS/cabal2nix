# Hackage2nix

This tool is used to update the `hackage-packages.nix` file in the nixpkgs repo.
Most of the time we will not need run this manually (since it is run automatically
on the upstream nixpkgs).  The cases when we would need to run this manually are:

* We have changed hackage2nix in some way and we want to check that it
behaves correctly.

* We have a fork of nixpkgs where we have added or changed
the name or location in nixpkgs of a native package used by a hackage package
and we want to check that hackage-packages.nix will be updated correctly.

**Important:** this must be run on a **case sensitive file system**.
On macOS you can use [this](https://gist.github.com/dixson3/8360571) to create one
(you can replace 60g with 4g as that should be plenty).

#### Setup
Replace `git@github.com:NixOS/nixpkgs.git` with a fork you have access to.
```
git clone git@github.com:NixOS/cabal2nix.git
cd cabal2nix
git clone git@github.com:NixOS/nixpkgs.git
git clone git@github.com:commercialhaskell/all-cabal-hashes.git
mv all-cabal-hashes hackage
cabal install
```

#### Running Hackage2nix
This will run hackage2nix and push a commit with the updated `hackage-packages.nix`
file to your fork of `nixpkgs`.
```
./update-nixpkgs.sh
```
