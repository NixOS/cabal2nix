# Revision History for cabal2nix

## 2.19.0

Note that some of the API has also changed in a breaking
manner because of the upgrade to [`distribution-nixpkgs`
1.7.0](https://github.com/nixos/distribution-nixpkgs/blob/v1.7.0/CHANGELOG.md#170),
see [#506](https://github.com/NixOS/cabal2nix/pull/506).

* Only use `hpack` when building if no cabal file is found
  for the package to process.
  See also [#508](https://github.com/NixOS/cabal2nix/pull/508).
* `hackage2nix` now supports arbitrary Nix-style platform tuples
  in `unsupported-platforms` (including the new `aarch64-darwin`) as
  well as nixpkgs platform groups which are denoted as e. g.
  `platforms.darwin` and can be used instead of platform tuples.
  See also [#506](https://github.com/NixOS/cabal2nix/pull/506).
  **API breaking change**: The `IsString` instance for `Platform` in
  `Distribution.Nixpkgs.Haskell.OrphanInstances` has been removed.
* The new `hackage2nix` `supported-platforms` configuration field
  allows prescribing a specific list of platforms to set in the
  package's `platforms` meta attribute. `unsupported-platforms`
  are now translated to `badPlatforms` instead of being subtracted
  from `platforms`.
  See also [#506](https://github.com/NixOS/cabal2nix/pull/506)
  and [#560](https://github.com/NixOS/cabal2nix/pull/560).
  **API Breaking Change** for
  `Distribution.Nixpkgs.Haskell.FromCabal.Configuration`.
* `cabal2nix` will no longer emit a dependency on `webkitgtk24x-gtk{2,3}`
  if it detects the older 3.0 API of WebKit being used. Nixpkgs hasn't
  contained this package for a few years now due to security
  vulnerabilities and the packages still using it on Hackage are
  unmaintained. If you have a legacy project built with an old
  version of nixpkgs, either don't upgrade `cabal2nix` or emulate
  the old behavior using overrides.
  See also [#521](https://github.com/NixOS/cabal2nix/pull/521).
* If the input cabal file declares just a single executable, the `mainProgram`
  meta attribute will be set in the resulting Nix expression.
  See also [#506](https://github.com/NixOS/cabal2nix/pull/506) and
  [#557](https://github.com/NixOS/cabal2nix/pull/557).
* If `cabal2nix` (or `hackage2nix`) doesn't recognize the license
  of a package, it'll still assume that it's free and enable building
  on Hydra (i. e. use the default value of `hydraPlatforms`).
  This is done because Hackage requires uploaded packages to
  be open source. You may need to keep this change in mind,
  however, if you use `cabal2nix` for packaging unfree
  software. See also [#520](https://github.com/NixOS/cabal2nix/pull/520).
  `isFreeLicense` has changed semantically as a result.
* Argument parsing logic in `cabal2nix` has been refactored
  in [#544](https://github.com/NixOS/cabal2nix/pull/544).
  **API breaking change** for the following modules:
  
  * `Cabal2nix`
  * `Distribution.Nixpkgs.Fetch`
  * `Distribution.Nixpkgs.Haskell.Derivation` (removed instance)
  * `Distribution.Nixpkgs.Haskell.PackageSourceSpec`
* Update handling of Lua bindings to reflect current state upstream.
  See [#527](https://github.com/NixOS/cabal2nix/pull/527) and
  [#547](https://github.com/NixOS/cabal2nix/pull/547).

## 2.18.0

* Support GHC 9.0.x and Cabal 3.4.0.0,
  **API breaking change**
  (See [#503](https://github.com/NixOS/cabal2nix/pull/503))
* Fix fetching remote _archives_: Previously they weren't properly
  unpacked, **API breaking change**
  (See [#455](https://github.com/NixOS/cabal2nix/pull/455))
* Resolution of system dependency attribute names in nixpkgs
  * Update changed upstream attribute names, reflect deprecations
  * Fix hackage2nix not resolving certain attribute names if multiple
    attribute names referred to the same derivation.
    (See [#492](https://github.com/NixOS/cabal2nix/pull/492) and
    [distribution-nixpkgs#10](https://github.com/NixOS/distribution-nixpkgs/pull/10))
* hackage2nix: accept multiple config files at the command
  line given by specifying `--config` more than once
  (See [#494](https://github.com/NixOS/cabal2nix/pull/494))
