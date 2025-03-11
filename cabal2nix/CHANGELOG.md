# Revision History for cabal2nix

## 2.20.0

* `cabal2nix` now [prints the commands it runs when fetching sources](https://github.com/nixOS/cabal2nix/commit/5327953d299eba0b6de4e88bacf4bba9022bb5e2).
* `cabal2nix` now [produces colorful output](https://github.com/NixOS/cabal2nix/pull/636)
  using ANSI SGR escape sequences when printing to a TTY.
  This can be disabled using the [`NO_COLOR` environment variable](https://no-color.org/)
  or forcibly enabled using `FORCE_COLOR`.
* `hackage2nix` now defaults internally to an utf-8 locale, ignoring enviroment
  variables.
* `cabal2nix` now exclusively uses the new
  [plural testTargets](https://github.com/NixOS/nixpkgs/pull/306283)
  argument.
  * This is an **API breaking change**: `_testTarget` was removed in
    favor of `_testTargets`.
  * Expressions using `_testTargets` will only work with
    Nixpkgs 24.11, 25.05 or newer.
    **Warning**: As of this writing (2025-03-10), the unstable channels of
    nixpkgs don't support testTargets yet. Inclusion is tracked by
    [nixpkgs#371032](https://github.com/NixOS/nixpkgs/pull/371032).
* Added support for the [testFlags argument](https://github.com/NixOS/nixpkgs/pull/126364),
  available since 21.11, to `Derivation`. **API breaking change**.
* Remove `_enableSplitObjs` from `Derivation` since it [hasn't been
  supported by Nixpkgs in a while](https://github.com/nixOS/nixpkgs/commit/a62b24be6d650757deb8fe222763f436a53650ff),
  **API breaking change**.
* Added and updated various library name to Nixpkgs attribute name mappings.
* Dropped/reduced post-processing hooks for
  * `pandoc`
  * `git-annex`
  * `haddock`
  * `tz`
* Postprocessing hooks utilising `replace` no longer fail when they can't find
  the binding to replace. This fixes an issue where `cabal2nix` would fail to
  generate an expression for a package when `hackage2nix` would succeed.

## 2.19.1

**Warning**: This version of `cabal2nix` generates Nix expressions that
are expected to be used with at least NixOS 22.11 or the unstable
channels at `3928cfa27d9925f9fbd1d211cf2549f723546a81` or later (which
should be all unstable channels published after 2022-10-23) due to changes
how `libcrypt` is provided by `nixpkgs`. For details read below.

* `cabal2nix` and `hackage2nix` will now emit a dependency on `libxcrypt`
  if the `crypt` library is requested by a package description. This has
  been done because `nixpkgs` has
  [disabled `libcrypt` being bundled with `glibc`](https://github.com/NixOS/nixpkgs/pull/181764),
  so the library will need to be pulled in from elsewhere.
  These new generated nix expressions may not work correctly with older
  versions of `nixpkgs` (since two `libcrypt`s would be pulled in).
  Versions expected to work are listed in the warning above.
  See also [#576](https://github.com/NixOS/cabal2nix/pull/576).
* `cabal2nix` and `hackage2nix` can now resolve the `FLAC` library name.
  See also [#567](https://github.com/NixOS/cabal2nix/pull/567).
* Fix test suite compilation with `Cabal >= 3.8`.
  See also [#572](https://github.com/NixOS/cabal2nix/pull/572).
* Packages closely tied to `cabal2nix` have been merged into the main
  `cabal2nix` source repository. As a consequence, the cabal2nix source
  code has been moved into the `cabal2nix` subdirectory.
  See also [#567](https://github.com/NixOS/cabal2nix/pull/567).

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
