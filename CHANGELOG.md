# Revision History for cabal2nix

## 2.19.0 (Unreleased)

* Only use `hpack` when building if no cabal file is found
  for the package to process
  (See [#508](https://github.com/NixOS/cabal2nix/pull/508))
* Support `aarch64-darwin` as an unsupported platform in
  `hackage2nix` (See [#517](https://github.com/NixOS/cabal2nix/pull/517))
* The new `hackage2nix` `supported-platforms` configuration field
  allows prescribing a specific list of platforms to set in the
  package's `platforms` meta attribute. `unsupported-platforms`
  are now translated to `badPlatforms` instead of being subtracted
  from `platforms`.
* `cabal2nix` will no longer emit a dependency on `webkitgtk24x-gtk{2,3}`
  if it detects the older 3.0 API of WebKit being used. Nixpkgs hasn't
  contained this package for a few years now due to security
  vulnerabilities and the packages still using it on Hackage are
  unmaintained. If you have a legacy project built with an old
  version of nixpkgs, either don't upgrade `cabal2nix` or emulate
  the old behavior using overrides.

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
