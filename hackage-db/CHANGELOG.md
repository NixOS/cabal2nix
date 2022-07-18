# Revision history for hackage-db

## 2.1.2

Fix a bug which lead to `parsePackageData` always failing if the package had
a `preferred-versions` file in the hackage tarball.
