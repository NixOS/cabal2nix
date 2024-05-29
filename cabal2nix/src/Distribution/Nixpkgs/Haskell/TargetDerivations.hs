{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Distribution.Nixpkgs.Haskell.TargetDerivations
  ( TargetDerivations, targetDerivations, allDerivations, libraries, exes, testExes, benchExes
  , nullTargetDerivations
  )
  where

import Prelude hiding ((<>))

import Control.DeepSeq
import Control.Lens
import Data.Set ( Set )
import qualified Data.Set as Set
import Distribution.Nixpkgs.Haskell.OrphanInstances ( )
import Distribution.Package
import GHC.Generics ( Generic )
import Distribution.Nixpkgs.Haskell.Derivation
import Language.Nix.PrettyPrinting

-- | A represtation of Nix expressions for building individual targets within a Haskell package.

data TargetDerivations = TargetDerivations
  { _libraries :: [Derivation]
  , _exes :: [Derivation]
  , _testExes :: [Derivation]
  , _benchExes :: [Derivation]
  }
  deriving (Show, Generic)

instance NFData TargetDerivations

makeLenses ''TargetDerivations

makeLensesFor [("_libraries", "allDerivations"), ("_executables", "allDerivations"), ("_testExecutables", "allDerivations"), ("_benchExecutables", "allDerivations")] ''TargetDerivations

targetDerivations :: Traversal' TargetDerivations Derivation
targetDerivations f (TargetDerivations {..}) = do
  libs <- traverse f _libraries
  exes' <- traverse f _exes
  testExes' <- traverse f _testExes
  benchExes' <- traverse f _benchExes
  pure TargetDerivations
    { _libraries = libs
    , _exes = exes'
    , _testExes = testExes'
    , _benchExes = benchExes'
    }

nullTargetDerivations :: TargetDerivations 
nullTargetDerivations = TargetDerivations
  { _libraries = mempty
  , _exes = mempty
  , _testExes = mempty
  , _benchExes = mempty
  }

allInputs :: TargetDerivations -> Set String
allInputs TargetDerivations {..} = Set.unions 
  [ mconcat $ inputs <$> _libraries
  , mconcat $ inputs <$> _exes
  , mconcat $ inputs <$> _testExes
  , mconcat $ inputs <$> _benchExes
  ]

instance Pretty TargetDerivations where
  pPrint targetDerivations = funargs (map text ("mkDerivation" : toAscList (allInputs targetDerivations))) $$ vcat
    [ text "let"
    , vcat $ derivationAttr <$> targetDerivations^.allDerivations
    , text "in" <+> lbrace
    , nest 2 $ text "inherit"
    , nest 4 $ vcat $ pPrint . packageName <$> targetDerivations^.allDerivations
    , nest 4 semi
    , rbrace
    ]

derivationAttr :: Derivation -> Doc
derivationAttr drv = attr (drvName drv) $ pPrint drv
    
drvName :: Derivation -> String
drvName drv = unPackageName $ packageName $ drv^.pkgid
