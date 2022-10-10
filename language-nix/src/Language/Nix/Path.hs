{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Nix.Path ( Path, path ) where

import Control.DeepSeq
import Control.Lens
import Data.Maybe
import Data.String
import GHC.Generics ( Generic )
import Language.Nix.Identifier
import Test.QuickCheck
import Text.Parsec.Class as P
import Text.PrettyPrint.HughesPJClass as PP

-- $setup
-- >>> import Control.Exception as Excpt

-- | Paths are non-empty lists of identifiers in Nix.
--
-- >>> path # [ident # "yo"]
-- Path [Identifier "yo"]
--
-- Any attempt to construct the empty path throws an 'error':
--
-- >>> :set -XScopedTypeVariables
-- >>> either (\(_::SomeException) -> "empty paths are illegal") show <$> Excpt.try (evaluate (path # []))
-- "empty paths are illegal"
--
-- Paths can be pretty-printed and parsed with the 'Text' class:
--
-- >>> parse "Path" "foo.\"foo.bar\".bar" :: Path
-- Path [Identifier "foo",Identifier "foo.bar",Identifier "bar"]
-- >>> pPrint (parse "Path" "foo.\"foo\".\"bar\".bar" :: Path)
-- foo.foo.bar.bar
--
-- prop> \p -> Just (p :: Path) == parseM "Path" (prettyShow p)
--
-- Paths are instances of strings and can be implicitly converted:
--
-- >>> :set -XOverloadedStrings
-- >>> pPrint $ ("yo.bar" :: Path)
-- yo.bar
-- >>> pPrint $ ("  yo  .  bar" :: Path)
-- yo.bar
--
-- Freaky quoted identifiers are fine throughout:
--
-- >>> pPrint $ path # ["yo","b\"ar"]
-- yo."b\"ar"
-- >>> pPrint ("\"5ident\"" :: Path)
-- "5ident"
-- >>> pPrint $ path # ["5ident","foo.bar","foo\nbar"]
-- "5ident"."foo.bar"."foo\nbar"

declareLenses [d| newtype Path = Path [Identifier]
                    deriving (Show, Eq, Ord, Generic)
              |]

instance NFData Path where
  rnf (Path p) = rnf p

instance Pretty Path where
  pPrint p = hcat $ punctuate (PP.char '.') (map pPrint (p^.path))

instance HasParser Path where
  parser = review path <$> (spaces >> parser) `sepBy` (spaces >> P.char '.')

instance IsString Path where
  fromString = parse "Language.Nix.Path.Path"

instance Arbitrary Path where
  arbitrary = Path <$> listOf1 arbitrary

instance CoArbitrary Path

-- | Use this isomorphism to construct a path from a list of identifiers, or to
-- access that list for a given path.

path :: Iso' Path [Identifier]
path = iso (\(Path p) -> p) (\p -> if null p then error "Nix paths cannot be empty" else Path p)
