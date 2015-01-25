{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Nix.Identifier ( Identifier, string, quotedString, needsQuoting ) where

import Data.Char
import Data.Function
import Control.Lens
import Data.String
import Distribution.Nix.Util.PrettyPrinting ( Pretty(..), text )
import Text.Regex.Posix

-- | Identifiers in Nix are essentially strings. Reasonable people restrict
-- themselves to identifiers of the form @[a-zA-Z\_][a-zA-Z0-9\_\'\-]*@,
-- though, because these don't need quoting. The @Identifier@ type is an
-- instance of the 'IsString' class for construction. The methods of the
-- 'Pretty' class can be used to pretty-print an identifier with proper
-- quoting.
--
-- >>> let i = fromString "test" :: Identifier in (i, pPrint i)
-- (Identifier "test",test)
-- >>> let i = fromString "foo.bar" :: Identifier in (i, pPrint i)
-- (Identifier "foo.bar","foo.bar")
--
-- The 'Ord' instance for identifiers is unusual in that it's aware of
-- character's case:
--
-- >>> (fromString "abc" :: Identifier) == fromString "abc"
-- True
-- >>> (fromString "abc" :: Identifier) == fromString "ABC"
-- False
-- >>> (fromString "abc" :: Identifier) > fromString "ABC"
-- True
-- >>> (fromString "abc" :: Identifier) > fromString "ABC"
-- True
-- >>> (fromString "X" :: Identifier) > fromString "a"
-- True
-- >>> (fromString "x" :: Identifier) > fromString "A"
-- True
--
-- prop> \str -> Identifier str == Identifier str
-- prop> \str -> any (`elem` ['a'..'z']) str ==> Identifier (map toLower str) /= Identifier (map toUpper str)
-- prop> \(NonEmpty str) -> head str `elem` ['a'..'z'] ==> Identifier (map toLower str) >= Identifier (map toUpper str)

newtype Identifier = Identifier String
  deriving (Show, Eq, IsString)

instance Pretty Identifier where
  pPrint i = text (i ^. string)

instance Ord Identifier where
  compare (Identifier a) (Identifier b) =
    case (compare `on` map toLower) a b of
      EQ -> compare a b
      r  -> r

needsQuoting :: Identifier -> Bool
needsQuoting (Identifier str) = not (str =~ "^[a-zA-Z\\_][a-zA-Z0-9\\_\\'\\-]*$")

-- | Lens that allows conversion from/to the standard 'String' type.
--
-- >>> putStrLn $ fromString "abc" ^. string
-- abc
--
-- >>> putStrLn $ fromString "abc.def" ^. string
-- "abc.def"
--
-- >>> pPrint $ fromString "" & string .~ "abcdef"
-- abcdef

string :: Lens' Identifier String
string f (Identifier str) = (\str' -> fromString str') `fmap` f s
  where s = if needsQuoting (Identifier str) then show str else str

-- | A variant of 'string' that gives read-only access to a string-representation
-- of the identifier that is always quoted, even if the identifier actually
-- doesn't need quoting.
--
-- >>> putStrLn $ fromString "abc" ^. quotedString
-- "abc"
--
-- >>> putStrLn $ fromString "abc.def" ^. quotedString
-- "abc.def"

quotedString :: Getter Identifier String
quotedString f (Identifier str) = fmap fromString (f (show str))
