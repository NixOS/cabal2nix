{-# LANGUAGE TemplateHaskell #-}

module Language.Nix.Binding ( Binding, binding, localName, reference ) where

import Internal.Lens
import Language.Nix.Identifier
import Language.Nix.Path
import Internal.PrettyPrinting

data Binding = Bind { _localName :: Identifier, _reference :: Path }
  deriving (Show, Eq, Ord)

makeLenses ''Binding

binding :: Lens' Binding (Identifier,Path)
binding f (Bind l r) = uncurry Bind `fmap` f (l, r)

instance Default Binding where
  def = Bind def def

instance Pretty Binding where
  pPrint b = case (init ps, last ps) of
               ([], i') -> if i == i'
                              then text "inherit" <+> pPrint i' <> semi
                              else pPrint i <+> equals <+> pPrint p <> semi
               (p', i') -> if i == i'
                              then text "inherit" <+> parens (pPrint (create path p')) <+> pPrint i' <> semi
                              else pPrint i <+> equals <+> pPrint p <> semi

    where
      (i, p) = view binding b
      ps = view path p
