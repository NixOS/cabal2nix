{-# LANGUAGE PatternGuards #-}
module Language.Nix.Path ( Path, mkPath, path ) where

import Control.Lens
import Distribution.Nixpkgs.Util.PrettyPrinting ( Pretty(..), punctuate, hcat, char )
import Language.Nix.Identifier

-- | Paths are non-empty lists of identifiers in Nix.

newtype Path = Path { _segments :: [Identifier] }
  deriving (Show, Eq, Ord)

mkPath :: [Identifier] -> Path
mkPath [] = error "Nix paths cannot be empty"
mkPath p
  | [Identifier s] <- p, needsQuoting s = error ("invalid Nix path: " ++ show p)
  | otherwise                           = Path p

path :: Lens' Path [Identifier]
path f (Path ids) = mkPath `fmap` f ids

instance Pretty Path where
  pPrint p = hcat $ punctuate (char '.') $ pPrint <$> p^.path
