# Revision History for language-nix

## 2.3.0 (unreleased)

* `Language.Nix.Identifier` now exports `nixKeywords` which lists
  keywords in Nix that are parseable as simple identifiers, but have
  special meaning, like `assert` and `with`. Consequently, they can't
  be used as (simple) identifiers in Nix code.

  `quote`, `needsQuoting` and `Pretty` will take this list into account
  and quote such identifiers. However, `HasParser` will _not_ reject them
  even if they are unquoted.
* Add an hspec/QuickCheck based test suite.
