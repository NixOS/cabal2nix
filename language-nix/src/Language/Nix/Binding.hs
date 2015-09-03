{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Nix.Binding ( Binding, binding, localName, reference ) where

import Control.DeepSeq
import Control.Lens
import Text.PrettyPrint.HughesPJClass
import Language.Nix.Identifier
import Language.Nix.Path

-- | A 'Binding' represents an identifier that refers to some other 'Path'.

declareLenses [d| data Binding = Bind { localName :: Identifier, reference :: Path }
                    deriving (Show, Eq, Ord)
              |]

binding :: Iso' Binding (Identifier,Path)
binding = iso (\(Bind l r) -> (l,r)) (uncurry Bind)

instance NFData Binding where rnf (Bind l r) = l `deepseq` rnf r

instance Pretty Binding where
  pPrint b = case (init ps, last ps) of
               ([], i') -> if i == i'
                              then text "inherit" <+> pPrint i' <> semi
                              else pPrint i <+> equals <+> pPrint p <> semi
               (p', i') -> if i == i'
                              then text "inherit" <+> parens (pPrint (path # p')) <+> pPrint i' <> semi
                              else pPrint i <+> equals <+> pPrint p <> semi

    where
      (i, p) = view binding b
      ps = view path p
