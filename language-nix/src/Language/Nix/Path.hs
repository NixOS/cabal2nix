{-# LANGUAGE DeriveGeneric #-}

module Language.Nix.Path ( Path, path ) where

import Data.Default.Class
import Control.Lens
import Text.PrettyPrint.HughesPJClass ( Pretty (..), char, hcat, punctuate )
import Language.Nix.Identifier
import Control.DeepSeq.Generics
import GHC.Generics ( Generic )

-- | Paths are non-empty lists of identifiers in Nix.
--
-- >>> set path [set ident "yo" undefined] undefined
-- Path {_segments = [Identifier "yo"]}
--
-- Any attempt to construct the empty path throws an 'error':
--
-- >>> set path [] undefined
-- Path {_segments = *** Exception: Nix paths cannot be empty
--
-- 'Identifier' is an instance of 'IsString':
--
-- >>> :set -XOverloadedStrings
-- >>> pPrint $ set path ["yo","bar"] undefined
-- yo.bar
--
-- Freaky quoted identifiers are fine except in the first segment:
--
-- >>> pPrint $ set path ["yo","b\"ar"] undefined
-- yo."b\"ar"
-- >>> pPrint $ set path ["5ident"] undefined
-- *** Exception: invalid Nix path: [Identifier "5ident"]
-- >>> pPrint $ set path ["5ident","foo","bar"] undefined
-- *** Exception: invalid Nix path: [Identifier "5ident",Identifier "foo",Identifier "bar"]

newtype Path = Path { _segments :: [Identifier] }
  deriving (Show, Eq, Ord, Generic)

instance NFData Path where rnf = genericRnf

instance Default Path where def = Path (error "undefined Nix.Path")

path :: Lens' Path [Identifier]
path f (Path ids) = mkPath `fmap` f ids
  where
    mkPath :: [Identifier] -> Path
    mkPath []                           = error "Nix paths cannot be empty"
    mkPath p@(s0:_)
      | s0^.ident.to needsQuoting       = error ("invalid Nix path: " ++ show p)
      | otherwise                       = Path p

instance Pretty Path where
  pPrint p = hcat $ punctuate (char '.') $ pPrint <$> p^.path
