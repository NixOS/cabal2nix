{-# LANGUAGE TemplateHaskell #-}

module Language.Nix.Path ( Path, path ) where

import Control.DeepSeq
import Control.Lens
import Language.Nix.Identifier
import Text.PrettyPrint.HughesPJClass ( Pretty (..), char, hcat, punctuate )

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
-- 'Identifier' is an instance of 'IsString':
--
-- >>> :set -XOverloadedStrings
-- >>> pPrint $ path # ["yo","bar"]
-- yo.bar
--
-- Freaky quoted identifiers are fine except in the first segment:
--
-- >>> pPrint $ path # ["yo","b\"ar"]
-- yo."b\"ar"
-- >>> pPrint $ path # ["5ident"]
-- *** Exception: invalid Nix path: [Identifier "5ident"]
-- >>> pPrint $ path # ["5ident","foo","bar"]
-- *** Exception: invalid Nix path: [Identifier "5ident",Identifier "foo",Identifier "bar"]

declareLenses [d| newtype Path = Path [Identifier]
                    deriving (Show, Eq, Ord)
              |]

instance NFData Path where rnf (Path p) = rnf p

instance Pretty Path where
  pPrint p = hcat $ punctuate (char '.') $ pPrint <$> p^.path

path :: Iso' Path [Identifier]
path = iso (\(Path p) -> p) mkPath
  where
    mkPath :: [Identifier] -> Path
    mkPath []                           = error "Nix paths cannot be empty"
    mkPath p@(s0:_)
      | s0^.ident.to needsQuoting       = error ("invalid Nix path: " ++ show p)
      | otherwise                       = Path p
