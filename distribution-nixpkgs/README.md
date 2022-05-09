# distribution-nixpkgs

[![hackage release](https://img.shields.io/hackage/v/distribution-nixpkgs.svg?label=hackage)](http://hackage.haskell.org/package/distribution-nixpkgs)
[![stackage LTS package](http://stackage.org/package/distribution-nixpkgs/badge/lts)](http://stackage.org/lts/package/distribution-nixpkgs)

## Maintainer's Notes

### Keeping platform test cases up to date

As nixpkgs expands its list of supported platforms, new system tuples are added.
Therefore we need to adjust the test suite regularly in order to cover newly
added platforms. For this purpose, the test suite checks the platforms it verifies
against a list of system tuples generated from nixpkgs. This list needs to be
updated manually, so running the test suite doesn't depend on Nix being available.

To check if any new platforms need to be covered, do the following:

1. Run `./test/data/regenerate-all-system-tuples.sh /path/to/nixpkgs/checkout`.
   If no local nixpkgs is given, `<nixpkgs>` will be used.

2. Run the test suite: `cabal v2-test`.

3. If the test suite fails, add test cases for all missing system tuples.

4. In all cases, change the dates of last update in `test/hspec.hs` and
   `src/Distribution/Nixpkgs/Meta.hs` to the current day.
