{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Nix.Identifier
  ( -- * Type-safe Identifiers
    Identifier, ident
  , parseSimpleIdentifier, parseQuotedIdentifier
    -- * String Predicates
  , needsQuoting
    -- * Internals
    -- TODO: only required by the language-nix test suite, unexport?
  , nixKeywords
  , quote
  )
  where

import Control.DeepSeq
import Control.Lens
import Data.Char
import Data.Either
import Data.String
import GHC.Generics ( Generic )
import Test.QuickCheck
import Text.Parsec.Class as P
import Text.PrettyPrint.HughesPJClass as PP

-- | Identifiers in Nix are essentially strings. They can be constructed
-- (and viewed) with the 'ident' isomorphism. For the sake of convenience,
-- @Identifier@s are an instance of the 'IsString' class.
--
-- It is usually wise to only use identifiers of the form
-- @[a-zA-Z_][a-zA-Z0-9_'-]*@, because these don't need quoting.
-- Consequently, they can appear almost anywhere in a Nix expression
-- (whereas quoted identifiers e.g. can't be used in function patterns).
-- The methods of the 'Pretty' class can be used to print an identifier
-- with proper quoting:
--
-- >>> pPrint (ident # "test")
-- test
-- >>> pPrint (ident # "foo.bar")
-- "foo.bar"
--
-- The 'HasParser' class allows parsing rendered identifiers even if they are
-- quoted:
--
-- >>> parseM "Identifier" "hello" :: Maybe Identifier
-- Just (Identifier "hello")
-- >>> parseM "Identifier" "\"3rd party\"" :: Maybe Identifier
-- Just (Identifier "3rd party")
--
-- __Warning__: Identifiers /may not/ contain @\'\\0\'@, but this is not
-- checked during construction!
--
-- See also <https://nix.dev/manual/nix/2.30/language/identifiers.html>.
declareLenses [d| newtype Identifier = Identifier { ident :: String }
                    deriving (Show, Eq, Ord, IsString, Generic)
              |]
-- ^ An isomorphism that allows conversion of 'Identifier' from/to the
-- standard 'String' type via 'review'.
--
-- >>> ident # "hello"
-- Identifier "hello"
-- >>> from ident # fromString "hello"
-- "hello"

instance NFData Identifier where
  rnf (Identifier str) = rnf str

instance Arbitrary Identifier where
  arbitrary = Identifier <$> oneof
    [ -- almost always needs quoting, unreasonable
      listOf1 (nonNul arbitraryUnicodeChar)
      -- almost always needs quoting, reasonable-ish
    , listOf1 (nonNul arbitraryPrintableChar)
      -- rarely needs quoting
    , listOf1 (arbitraryASCIIChar `suchThat` isSimpleChar) ]
    where nonNul g = g `suchThat` (/= '\0')
          isSimpleChar c = isAlphaNum c || c `elem` "_-'"
  shrink (Identifier i) = map Identifier (shrink i)

instance CoArbitrary Identifier

instance Pretty Identifier where
  pPrint = view (ident . to quote . to text)

-- | Note that this parser is more lenient than Nix w.r.t. simple identifiers,
--   since it will accept 'nixKeywords'.
--
--   Naturally, it does not support string interpolation, but does not reject
--   strings that contain them. E.g. the string literal @"hello ${world}"@
--   will contain @${world}@ verbatim after parsing. Do not rely on this
--   behavior, as it may be changed in the future.
instance HasParser Identifier where
  parser = parseQuotedIdentifier <|> parseSimpleIdentifier

-- | Parsec parser for simple identifiers, i.e. those that don't need quoting.
--   The parser is equivalent to the regular expression @^[a-zA-Z_][a-zA-Z0-9_'-]*$@
--   which the Nix parser uses.
--
--   Note that this parser will accept keywords which would not be parsed as
--   identifiers by Nix, see 'nixKeywords'.
parseSimpleIdentifier :: CharParser st tok m Identifier
parseSimpleIdentifier = do
  c <- satisfy (\x -> x == '_' || (isAscii x && isAlpha x))
  cs <- many (satisfy (\x -> x `elem` "_'-" || (isAscii x && isAlphaNum x)))
  return (Identifier (c:cs))

-- | 'ReadP' parser for quoted identifiers, i.e. those that /do/ need
-- quoting.
parseQuotedIdentifier :: CharParser st tok m Identifier
parseQuotedIdentifier = Identifier <$> qstring
  where
    qstring :: CharParser st tok m String
    qstring = between (P.char '"') (P.char '"') (many qtext)

    qtext :: CharParser st tok m Char
    qtext = quotedPair <|> P.noneOf "\\\""

    quotedPair :: CharParser st tok m Char
    quotedPair = do
      _ <- P.char '\\'
      c <- anyChar
      -- See https://github.com/NixOS/nix/blob/2d83bc6b83763290e9bbf556209927ba469956aa/src/libexpr/lexer.l#L54-L60
      return $ case c of
                 'n' -> '\n'
                 't' -> '\t'
                 'r' -> '\r'
                 -- Note that this handles actual escapes like \" and \\ and
                 -- bogus cases like \f which Nix doesn't fail on (despite not
                 -- supporting it), but simply maps to plain f
                 _ -> c

-- | Checks whether a given string needs quoting when interpreted as an
-- 'Identifier'.
needsQuoting :: String -> Bool
needsQuoting s =
  s `elem` nixKeywords
  || isLeft (runParser (parseSimpleIdentifier >> eof) () "" s)

-- | List of strings that are parseable as simple identifiers (see
--   'parseSimpleIdentifier') in isolation, but won't be accepted by Nix because
--   [keywords](https://nix.dev/manual/nix/2.30/language/identifiers.html#keywords)
--   take precedence.
nixKeywords :: [String]
nixKeywords =
  [ "assert", "with", "if", "then", "else", "let", "in", "rec", "inherit", "or" ]

-- | Helper function to quote a given identifier string if necessary.
--   Usually, one should use the 'Pretty' instance of 'Identifier' instead.
--
-- >>> putStrLn (quote "abc")
-- abc
-- >>> putStrLn (quote "abc.def")
-- "abc.def"
-- >>> putStrLn (quote "$foo")
-- "$foo"
-- >>> putStrLn (quote "${foo}")
-- "\${foo}"
quote :: String -> String
quote s = if needsQuoting s then '"' : quote' s else s
  where
    quote' (c1:c2:cs) = escapeChar c1 (Just c2) ++ quote' (c2:cs)
    quote' (c:cs) = escapeChar c Nothing ++ quote' cs
    quote' "" = "\""

escapeChar :: Char -> Maybe Char -> String
escapeChar c1 c2 =
  case c1 of
    -- supported escape sequences, see quotedPair above
    -- N.B. technically, we only need to escape \r (since Nix converts raw \r to \n),
    -- but it's nicer to escape what we can.
    '\n' -> "\\n"
    '\t' -> "\\t"
    '\r' -> "\\r"
    -- syntactically significant in doubly quoted strings
    '\\' -> "\\\\"
    '"' -> "\\\""
    '$' | c2 == Just '{' -> "\\$"
    _ -> [c1]
