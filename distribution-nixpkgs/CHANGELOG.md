# Revision history for distribution-nixpkgs

## 1.7.1.1

* Adapt test suite to added representation of `RISCV64` and
  `LoongArch64` architectures in Cabal 3.12.

## 1.7.1

* GHC and nixpkgs now
  [use the system double `javascript-ghcjs`](https://github.com/NixOS/nixpkgs/commit/471b9cab41b218080f5e9f4abbc83eaaa60c6abf)
  instead of `js-ghcjs` to match Cabal's default rendering. With this release,
  `distribution-nixpkgs` does the same.
  Note that this means that this release needs to be used with nixpkgs unstable
  after [97d55ec](https://github.com/NixOS/nixpkgs/commit/97d55ec923b0cd3798a6a84e2e0a6b2c6b54f6a9)
  or nixpkgs stable 23.05 or newer.

## 1.7.0.1

* Adapt test suite to changed representation of some `Platform`s in Cabal 3.8.
* Update test data to include new nixpkgs architectures `rx`, `microblaze` and
  `microblazeel`.
* The distribution-nixpkgs repository has been merged into the
  [cabal2nix](https://github.com/NixOS/cabal2nix) repository.
  All URLs in the cabal file have been updated and the distribution-nixpkgs
  repository will be archived.

## 1.7.0

* `Distribution.Nixpkgs.Meta`
  * `pPrint (x :: Meta)` now renders every maintainer as a full attribute
    path instead of using `with`.
  * **API breaking change**: Changed types for all platform related fields /
    lenses of `Meta`, i.e. `platforms` and `hydraPlatforms`.
    * They are now wrapped in a `Maybe`. If `Nothing`, the respective attribute
      is not rendered.
    * Instead of `Platform` we now use `NixpkgsPlatform` which may also be a
      `NixpkgsPlatformGroup`, like `lib.platforms.linux`, in addition to the
      wrapped `Platform` in `NixpkgsPlatformSingle`.
  * **API breaking change**: Add new fields / lenses to `Meta`:
    * `badPlatforms`: Allows setting a list of unsupported platforms, works
      similarly to the `platform` / `hydraPlatform` lenses.
    * `mainProgram`: Allows setting the (base-)name of the main binary of a
      derivation. Not rendered if `Nothing`.
  * **API breaking change**: Remove `allKnownPlatforms`, its previous use can be
    replaced using `badPlatforms`.
  * Added `nixpkgsPlatformFromString` which parses a specific format used by
    hackage2nix's config files into either `NixpkgsPlatformSingle` or
    `NixpkgsPlatformGroup`.
* `Language.Nix.PrettyPrinting`: Added new helpers, `listattrDoc` and
  `toAscListSortedOn`.

## 1.6.2

* Expect `MonadFailDesugaring` (or equivalent behavior) to be the default,
  requiring GHC >= 8.6.1. Adds support for GHC 9.2.1 (rc1).

## 1.6.1

* `Distribution.Nixpkgs.Meta`: support `aarch64-darwin`
  * Render `Platform AArch64 OSX` to `"aarch64-darwin"`
  * Add `Platform AArch64 OSX` to `allKnownPlatforms`

## 1.6.0

* `Distribution.Nixpkgs.PackageMap`
  * `readNixpkgPackageMap`: instead of a list of arguments to pass to
    `nix-env`, take path to nixpkgs and an optional nix expression
    to pass to it as arguments.
  * `readNixpkgPackageMap`: populate `PackageMap` with *all* attribute
    paths that point to derivations instead of just one per derivation.
    This fixes `resolve` not finding certain identifiers if there were
    two attributes pointing to the same derivation in nixpkgs. See also
    [#9](https://github.com/NixOS/distribution-nixpkgs/issues/9).
