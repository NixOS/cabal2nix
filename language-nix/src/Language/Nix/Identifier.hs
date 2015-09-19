{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Nix.Identifier
  (
    -- | Identifiers in Nix are essentially strings. They can be constructed
    -- (and viewed) with the 'ident' isomorphism. For the sake of convenience,
    -- @Identifier@s are an instance of the 'IsString' class.
    --
    -- Reasonable people restrict themselves to identifiers of the form
    -- @[a-zA-Z_][a-zA-Z0-9_'-]*@, because these don't need quoting. The
    -- methods of the 'Text' class can be used to parse and pretty-print an
    -- identifier with proper quoting:
    --
    -- >>> disp (ident # "test")
    -- test
    -- >>> disp (ident # "foo.bar")
    -- "foo.bar"
    --
    -- prop> \str -> Just (ident # str) == simpleParse (quote str)
    -- prop> \i -> Just (i :: Identifier) == simpleParse (display i)
    Identifier

  , -- | An isomorphism that allows conversion of 'Identifier' from/to the
    -- standard 'String' type via 'review'.
    --
    -- prop> \str -> fromString str == ident # str
    -- prop> \str -> set ident str undefined == ident # str
    -- prop> \str -> view ident (review ident str) == str
    ident

  , -- | Helper function to quote a given identifier string if necessary.
    --
    -- >>> putStrLn (quote "abc")
    -- abc
    -- >>> putStrLn (quote "abc.def")
    -- "abc.def"
    quote

  , -- | Checks whether a given string needs quoting when interpreted as an
    -- 'Identifier'. Simple identifiers that don't need quoting match the
    -- regular expression @^[a-zA-Z_][a-zA-Z0-9_'-]*$@.
    needsQuoting

  , -- | 'ReadP' parser for simple identifiers, i.e. those that don't need
    -- quoting.
    parseSimpleIdentifier

  , -- | 'ReadP' parser for quoted identifiers, i.e. those that /do/ need
    -- quoting.
    parseQuotedIdentifier

  ) where

import Control.DeepSeq
import Control.Lens
import Data.Char
import Data.String
import Distribution.Compat.ReadP as ReadP
import Distribution.Text
import GHC.Generics ( Generic )
import Prelude.Compat
import Test.QuickCheck
import Text.PrettyPrint as PP

declareLenses [d| newtype Identifier = Identifier { ident :: String }
                    deriving (Show, Eq, Ord, IsString, Generic)
              |]

instance NFData Identifier where
  rnf (Identifier str) = rnf str

instance Arbitrary Identifier where
  arbitrary = Identifier <$> arbitrary
  shrink (Identifier i) = map Identifier (shrink i)

instance Text Identifier where
  disp = view (ident . to quote . to text)
  parse = parseQuotedIdentifier <++ parseSimpleIdentifier

parseSimpleIdentifier :: ReadP r Identifier
parseSimpleIdentifier = do
  c <- satisfy (\x -> x == '_' || isAlpha x)
  cs <- munch (\x -> x `elem` "_'-" || isAlphaNum x)
  return (Identifier (c:cs))

parseQuotedIdentifier :: ReadP r Identifier
parseQuotedIdentifier = Identifier . read . fst
                     <$> gather (between (ReadP.char '"') (ReadP.char '"') (many qString))
  where
    qString :: ReadP r String
    qString = quotedPair <++ munch1 (`notElem` "\\\"")

    quotedPair :: ReadP r String
    quotedPair = do
      c1 <- ReadP.char '\\'
      c2 <- get
      return [c1,c2]

needsQuoting :: String -> Bool
needsQuoting s = null r || not (any (null . snd) r)
  where r = readP_to_S parseSimpleIdentifier s

quote :: String -> String
quote s = if needsQuoting s then show s else s
