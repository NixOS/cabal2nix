# Revision history for hackage-db

## 2.1.3

* `hackageTarball` / `cabalStateDir` now support overriding the cabal directory
  location by setting the `CABAL_DIR` environment variable. This is useful if
  `hackage-db` doesn't detect the correct location on its own:

  - A matching cabal state directory may exist, but should not be used for some
    reason.

  - A non-standard cabal state directory may be used, but `hackage-db` can't
    find it (as it doesn't check the `cabal-install` configuration file).

* `hackageTarball` now supports all state dir location(s) (newly) supported by
  `cabal-install`. If `CABAL_DIR` is not set, it will look in the following
  locations in that order:

  1. `$HOME/.cabal`, the classic location, will be preferred if it exists.
  2. `$XDG_CACHE_HOME/cabal` (usually `$HOME/.cache/cabal`) is used otherwise.
     `cabal-install` 3.10.1.0 and newer will default to this location for
     fresh installations.

## 2.1.2

Fix a bug which lead to `parsePackageData` always failing if the package had
a `preferred-versions` file in the hackage tarball.
