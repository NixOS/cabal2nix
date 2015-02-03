{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Nix.Identifier ( Identifier(..), ident, quote, needsQuoting ) where

import Control.Lens
import Data.Char
import Data.Function ( on )
import Data.String
import Distribution.Nixpkgs.Util.PrettyPrinting ( Pretty(..), text )
import Text.Regex.Posix

-- | Identifiers in Nix are essentially strings. Reasonable people restrict
-- themselves to identifiers of the form @[a-zA-Z\_][a-zA-Z0-9\_\'\-]*@,
-- because these don't need quoting. The @Identifier@ type is an instance of
-- the 'IsString' class for convenience. The methods of the 'Pretty' class can
-- be used to pretty-print an identifier with proper quoting.
--
-- >>> let i = Identifier "test" in (i, pPrint i)
-- (Identifier "test",test)
-- >>> let i = Identifier "foo.bar" in (i, pPrint i)
-- (Identifier "foo.bar","foo.bar")
--
-- The 'Ord' instance for identifiers is unusual in that it's aware of
-- character's case:
--
-- >>> Identifier "abc" == Identifier "ABC"
-- False
-- >>> Identifier "abc" > Identifier "ABC"
-- True
-- >>> Identifier "abc" > Identifier "ABC"
-- True
-- >>> Identifier "X" > Identifier "a"
-- True
-- >>> Identifier "x" > Identifier "A"
-- True
--
-- prop> \str -> Identifier str == Identifier str
-- prop> \str -> any (`elem` ['a'..'z']) str ==> Identifier (map toLower str) /= Identifier (map toUpper str)

newtype Identifier = Identifier String
  deriving (Show, Eq, IsString)

instance Pretty Identifier where
  pPrint i = text (i ^. ident . to quote)

instance Ord Identifier where
  compare (Identifier a) (Identifier b) =
    case (compare `on` map toLower) a b of
      EQ -> compare a b
      r  -> r

-- | Checks whether a given string would need quoting when interpreted as an
-- intentifier.

needsQuoting :: String -> Bool
needsQuoting str = not (str =~ grammar)
  where grammar :: String       -- TODO: should be a compiled regular expression
        grammar = "^[a-zA-Z\\_][a-zA-Z0-9\\_\\'\\-]*$"

-- | Lens that allows conversion from/to the standard 'String' type. The setter
-- does not evaluate its input, so it's safe to use with 'undefined'.
--
-- >>> putStrLn $ Identifier "abc.def" ^. ident
-- abc.def
--
-- >>> pPrint $ undefined & ident .~ "abcdef"
-- abcdef

ident :: Lens' Identifier String
ident f (Identifier str) = Identifier `fmap` f str

-- | Help function that quotes a given identifier string (if necessary).
--
-- >>> putStrLn (quote "abc")
-- abc
--
-- >>> putStrLn (quote "abc.def")
-- "abc.def"

quote :: String -> String
quote s = if needsQuoting s then show s else s
