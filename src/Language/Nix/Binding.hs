{-# LANGUAGE TemplateHaskell #-}

module Language.Nix.Binding ( Binding, localName, reference ) where

import Control.Lens
import Language.Nix.Identifier
import Language.Nix.Path

data Binding = Bind { _localName :: Identifier, _reference :: Path }
  deriving (Show, Eq, Ord)

makeLenses ''Binding
