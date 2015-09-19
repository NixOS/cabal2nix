{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Nix.Path ( Path, path ) where

import Control.DeepSeq
import Control.Lens
import Data.Maybe
import Data.String
import Distribution.Compat.ReadP as ReadP
import Distribution.Text
import GHC.Generics ( Generic )
import Language.Nix.Identifier
import Prelude.Compat
import Test.QuickCheck
import Text.PrettyPrint as PP

-- | Paths are non-empty lists of identifiers in Nix.
--
-- >>> path # [ident # "yo"]
-- Path [Identifier "yo"]
--
-- Any attempt to construct the empty path throws an 'error':
--
-- >>> path # []
-- Path *** Exception: Nix paths cannot be empty
--
-- Paths can be pretty-printed and parsed with the 'Text' class:
--
-- >>> simpleParse "foo.\"foo.bar\".bar" :: Maybe Path
-- Just (Path [Identifier "foo",Identifier "foo.bar",Identifier "bar"])
-- >>> maybe empty disp (simpleParse "foo.\"foo\".\"bar\".bar" :: Maybe Path)
-- foo.foo.bar.bar
--
-- prop> \p -> Just (p :: Path) == simpleParse (display p)
--
-- Paths are instances of strings and can be implicitly converted:
--
-- >>> :set -XOverloadedStrings
-- >>> disp $ ("yo.bar" :: Path)
-- yo.bar
-- >>> disp $ ("  yo  .  bar  " :: Path)
-- yo.bar
--
-- Freaky quoted identifiers are fine throughout:
--
-- >>> disp $ path # ["yo","b\"ar"]
-- yo."b\"ar"
-- >>> disp ("\"5ident\"" :: Path)
-- "5ident"
-- >>> disp $ path # ["5ident","foo.bar","foo\nbar"]
-- "5ident"."foo.bar"."foo\nbar"

declareLenses [d| newtype Path = Path [Identifier]
                    deriving (Show, Eq, Ord, Generic)
              |]

instance NFData Path where
  rnf (Path p) = rnf p

instance Text Path where
  disp p = hcat $ punctuate (PP.char '.') (map disp (p^.path))
  parse = review path <$> sepBy (skipSpaces *> parse) (skipSpaces *> ReadP.char '.')

instance IsString Path where
  fromString s = fromMaybe (error ("invalid Nix path: " ++ s)) (simpleParse s)

instance Arbitrary Path where
  arbitrary = Path <$> listOf1 arbitrary

path :: Iso' Path [Identifier]
path = iso (\(Path p) -> p) (\p -> if null p then error "Nix paths cannot be empty" else Path p)
