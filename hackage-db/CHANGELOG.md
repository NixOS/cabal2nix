# Revision history for hackage-db

## 2.2.0 (unreleased)

* `Distribution.Hackage.DB.Parsed.parsePackageData` no longer filters out
  package versions that are not preferred. This means that the list of
  available versions will no longer differ between the `Parsed` and
  `Unparsed` modules. This is a **breaking change**.
  To restore the old behavior, use the `preferred` field of `VersionData`
  to filter the `versions` map (see below).
* `Distribution.Hackage.DB.Parsed.PackageData` has been changed from a type
  alias to a sum type. Its previous content is the new `versions` field.
  This is an **API breaking change**.
* `Distribution.Hackage.DB.Parsed.VersionData` gains a new field, `preferred`
  which describes whether the package version in question matches the
  `preferred-versions` range given also exposed via the `preferredVersions`
  field of `Distribution.Hackage.DB.Parsed.PackageData`.
  This is an **API breaking change**.

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
