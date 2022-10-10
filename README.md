# The cabal2nix monorepo

![Continous Integration](https://github.com/NixOS/cabal2nix/workflows/Haskell-CI/badge.svg)

## Components

### cabal2nix

Tool that generates Nix build instructions from a Cabal file. Also contains
hackage2nix, the tool that generates the `haskellPackages` set in nixpkgs.

### distribution-nixpkgs

Utility library implementing nixpkgs-specific tasks and concepts: Looking
up packages, `meta` sets, Nix-style integrity hashes etc.

### language-nix

Simplistic library to parse and render a subset of the Nix language.

### hackage-db

Library for working with the Hackage database created and updated
using `cabal update`.

## Development

At the top-level, a `cabal.project` and `shell.nix` are provided for working
on all packages in the repository. You can use `direnv` and (optionally)
lorri to make the environment available in your normal shell as well.

## Maintenance

The monorepo has been assembled using [josh]. You can obtain it from nixpkgs
using `nix-shell -p josh`. Below, some common tasks are documented.

### Extracting the original git history

[josh]'s history filtering capabilities are quite powerful, allowing us to
extract the original git histories of the repositories that have been vendored
in. For example, for `distribution-nixpkgs`:

```console
$ josh-filter ':/distribution-nixpkgs'
$ git checkout FILTERED_HEAD
$ ls *.cabal
distribution-nixpkgs.cabal
```

The `FILTERED_HEAD` ref has the original `HEAD` of the `distribution-nixpkgs`
`HEAD` when it was vendored as its ancestor and can thus be pushed to the
original repository.


### Vendoring components

This probably won't come up in the future, but the process that was used to
vendor the additional libraries is documented here:

```console
$ name=…
$ git fetch https://github.com/nixos/$name.git
$ josh-filter ":prefix=$name" FETCH_HEAD
$ git merge --allow-unrelated-histories FILTERED_HEAD -m "$name: subtree upstream repo"
```

Also refer to the relevant [josh
documentation](https://josh-project.github.io/josh/guide/importing.html).

[josh]: https://josh-project.github.io/josh/
