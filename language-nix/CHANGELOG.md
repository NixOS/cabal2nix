# Revision History for language-nix

## 2.3.0 (unreleased)

* `Language.Nix.Identifier` now exports `nixKeywords` which lists
  keywords in Nix that are parseable as simple identifiers, but have
  special meaning, like `assert` and `with`. Consequently, they can't
  be used as (simple) identifiers in Nix code.

  `quote`, `needsQuoting` and `Pretty` will take this list into account
  and quote such identifiers. However, `HasParser` will _not_ reject them
  even if they are unquoted.
* Resolved discrepancies between `Language.Nix.Identifier` and Nix w.r.t.
  quoting and escaping:

  - Fixed missing escaping of some Nix syntax elements, e.g. in the case of
   `ident # "${foo}"`.
  - Pretty printing `Identifier`s will no longer produce escape sequences
    Haskell supports, but Nix doesn't.
  - Parsing `Identifier`s won't interpret escape sequences that Nix wouldn't
    understand.
* Added an hspec/QuickCheck based test suite.
