# Revision History for language-nix

## 2.3.0 (unreleased)

* `Language.Nix.Identifier` now exports `illegalSimpleIdentifiers` which
  lists strings that are parseable as simple identifiers, but clash with
  Nix keywords like `assert` and `with`.

  `quote`, `needsQuoting` and `Pretty` will take this list into account
  and quote such identifiers. However, `HasParser` will not reject them
  even if they are unquoted.
* Add an hspec/QuickCheck based test suite.
