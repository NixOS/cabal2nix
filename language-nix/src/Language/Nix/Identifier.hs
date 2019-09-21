{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Nix.Identifier
  ( Identifier, ident, quote, needsQuoting
  , parseSimpleIdentifier, parseQuotedIdentifier
  )
  where

import Control.DeepSeq
import Control.Lens
import Data.Char
import Data.Either
import Data.String
import GHC.Generics ( Generic )
import Prelude.Compat
import Test.QuickCheck
import Text.Parsec.Class as P
import Text.PrettyPrint.HughesPJClass as PP hiding ( (<>) )

-- | Identifiers in Nix are essentially strings. They can be constructed
-- (and viewed) with the 'ident' isomorphism. For the sake of convenience,
-- @Identifier@s are an instance of the 'IsString' class.
--
-- Reasonable people restrict themselves to identifiers of the form
-- @[a-zA-Z_][a-zA-Z0-9_'-]*@, because these don't need quoting. The
-- methods of the 'Text' class can be used to parse and pretty-print an
-- identifier with proper quoting:
--
-- >>> pPrint (ident # "test")
-- test
-- >>> pPrint (ident # "foo.bar")
-- "foo.bar"
--
-- prop> \str -> Just (ident # str) == parseM "Ident" (quote str)
-- prop> \i -> Just (i :: Identifier) == parseM "Ident" (prettyShow i)
declareLenses [d| newtype Identifier = Identifier { ident :: String }
                    deriving (Show, Eq, Ord, IsString, Generic)
              |]
-- ^ An isomorphism that allows conversion of 'Identifier' from/to the
-- standard 'String' type via 'review'.
--
-- prop> \str -> fromString str == ident # str
-- prop> \str -> set ident str undefined == ident # str
-- prop> \str -> view ident (review ident str) == str

instance NFData Identifier where
  rnf (Identifier str) = rnf str

instance Arbitrary Identifier where
  arbitrary = Identifier <$> listOf1 arbitraryUnicodeChar
  shrink (Identifier i) = map Identifier (shrink i)

instance CoArbitrary Identifier

instance Pretty Identifier where
  pPrint = view (ident . to quote . to text)

instance HasParser Identifier where
  parser = parseQuotedIdentifier <|> parseSimpleIdentifier

-- | Parsec parser for simple identifiers, i.e. those that don't need quoting.
parseSimpleIdentifier :: CharParser st tok m Identifier
parseSimpleIdentifier = do
  c <- satisfy (\x -> x == '_' || isAlpha x)
  cs <- many (satisfy (\x -> x `elem` "_'-" || isAlphaNum x))
  return (Identifier (c:cs))

-- | 'ReadP' parser for quoted identifiers, i.e. those that /do/ need
-- quoting.
parseQuotedIdentifier :: CharParser st tok m Identifier
parseQuotedIdentifier = Identifier <$> qstring
  where
    qstring :: CharParser st tok m String
    qstring = do txt <- between (P.char '"') (P.char '"') (many qtext)
                 return (read ('"' : concat txt <> ['"']))

    qtext :: CharParser st tok m String
    qtext = quotedPair <|> many1 (P.noneOf "\\\"")

    quotedPair :: CharParser st tok m String
    quotedPair = do
      c1 <- P.char '\\'
      c2 <- anyChar
      return [c1,c2]

-- | Checks whether a given string needs quoting when interpreted as an
-- 'Identifier'. Simple identifiers that don't need quoting match the
-- regular expression @^[a-zA-Z_][a-zA-Z0-9_'-]*$@.
needsQuoting :: String -> Bool
needsQuoting = isLeft . runParser (parseSimpleIdentifier >> eof) () ""

-- | Helper function to quote a given identifier string if necessary.
--
-- >>> putStrLn (quote "abc")
-- abc
-- >>> putStrLn (quote "abc.def")
-- "abc.def"
quote :: String -> String
quote s = if needsQuoting s then show s else s
