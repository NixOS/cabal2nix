{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Nix.Path where

import Control.Lens
import Data.String
import Distribution.Nix.Util.PrettyPrinting -- ( Pretty(..), text )
import Language.Nix.Identifier

-- | Paths are non-empty lists of identifiers in Nix.

newtype Path = Path [Identifier]
  deriving (Show, Eq)
