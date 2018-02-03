{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Distribution.Nixpkgs.Haskell.BuildInfo
  ( BuildInfo
  , haskell, pkgconfig, system, tool, pPrintBuildInfo
  )
  where

import Control.DeepSeq
import Control.Lens
import Data.Semigroup
import Data.Set ( Set )
import Data.Set.Lens
import GHC.Generics ( Generic )
import Language.Nix
import Language.Nix.PrettyPrinting hiding ( (<>) )

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

instance Semigroup BuildInfo where
  BuildInfo w1 x1 y1 z1 <> BuildInfo w2 x2 y2 z2 =
    BuildInfo (w1 <> w2) (x1 <> x2) (y1 <> y2) (z1 <> z2)

instance Monoid BuildInfo where
  mempty = BuildInfo mempty mempty mempty mempty
  mappend = (<>)

instance NFData BuildInfo

pPrintBuildInfo :: String -> BuildInfo -> Doc
pPrintBuildInfo prefix bi = vcat
  [ setattr (prefix++"HaskellDepends") empty (setOf (haskell.folded.localName.ident) bi)
  , setattr (prefix++"SystemDepends")  empty (setOf (system.folded.localName.ident) bi)
  , setattr (prefix++"PkgconfigDepends") empty (setOf (pkgconfig.folded.localName.ident) bi)
  , setattr (prefix++"ToolDepends") empty (setOf (tool.folded.localName.ident) bi)
  ]
