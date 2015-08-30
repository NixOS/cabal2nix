{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Distribution.Nixpkgs.Haskell.BuildInfo
  ( BuildInfo
  , haskell, pkgconfig, system, tool, pPrintBuildInfo
  )
  where

import Control.DeepSeq.Generics
import Data.Set ( Set )
import Data.Set.Lens
import GHC.Generics ( Generic )
import Text.PrettyPrint.HughesPJClass
import Language.Nix
import Control.Lens
import Internal.PrettyPrinting

data BuildInfo = BuildInfo
  { _haskell   :: Set Binding
  , _pkgconfig :: Set Binding
  , _system    :: Set Binding
  , _tool      :: Set Binding
  }
  deriving (Show, Eq, Generic)

makeLenses ''BuildInfo

instance Each BuildInfo BuildInfo (Set Binding) (Set Binding) where
  each f (BuildInfo a b c d) = BuildInfo <$> f a <*> f b <*> f c <*> f d

instance Monoid BuildInfo where
  mempty = BuildInfo mempty mempty mempty mempty
  BuildInfo w1 x1 y1 z1 `mappend` BuildInfo w2 x2 y2 z2 = BuildInfo (w1 `mappend` w2) (x1 `mappend` x2) (y1 `mappend` y2) (z1 `mappend` z2)

instance NFData BuildInfo where rnf = genericRnf

pPrintBuildInfo :: String -> BuildInfo -> Doc
pPrintBuildInfo prefix bi = vcat
  [ setattr (prefix++"HaskellDepends") empty (setOf (haskell.folded.localName.ident) bi)
  , setattr (prefix++"SystemDepends")  empty (setOf (system.folded.localName.ident) bi)
  , setattr (prefix++"PkgconfigDepends") empty (setOf (pkgconfig.folded.localName.ident) bi)
  , setattr (prefix++"ToolDepends") empty (setOf (tool.folded.localName.ident) bi)
  ]
