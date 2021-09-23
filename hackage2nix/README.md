# `hackage2nix`

#### Usage

This tool is used to update the Haskell package set in
[nixpkgs](https://github.com/NixOS/nixpkgs)
([`hackage-packages.nix`](https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/hackage-packages.nix)).

It gets used by the
[`maintainers/scripts/haskell/regenerate-hackage-packages.sh`](https://github.com/NixOS/nixpkgs/blob/haskell-updates/maintainers/scripts/haskell/regenerate-hackage-packages.sh)
script in nixpkgs. There are usually no reasons to run this tool any other way.
For development and testing you can modify the environment variable HACKAGE2NIX
of that script.

Further informations regarding the usage of hackage2nix in nixpkgs can be found under [nixpkgs:pkgs/development/haskell-modules/HACKING.md](https://github.com/NixOS/nixpkgs/blob/haskell-updates/pkgs/development/haskell-modules/HACKING.md).

#### Requirements

This must be run on a **case sensitive file system**.
On OSX you can use [this](https://gist.github.com/dixson3/8360571) to create one
(you can replace `60g` with `4g` as that should be plenty).

For building `cabal2nix` and `hackage2nix` you need to have `cabal-install`
installed.

#### Development

For development setup see the toplevel cabal2nix README.

#### Video tutorial

There is a now out-dated [video on YouTube](https://www.youtube.com/watch?v=qX0mgtSm360)
that gives a thorough walk-through of the previous work flow. It also explains many of the background considerations.
