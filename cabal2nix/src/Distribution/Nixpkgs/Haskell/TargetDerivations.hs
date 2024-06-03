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

makeLensesFor [("_libraries", "allDerivations"), ("_exes", "allDerivations"), ("_testExes", "allDerivations"), ("_benchExes", "allDerivations")] ''TargetDerivations

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
allInputs TargetDerivations {..} = Set.filter (`notElem` internalLibNames) $ Set.unions $ mconcat
  [ inputs <$> _libraries
  , inputs <$> _exes
  , inputs <$> _testExes
  , inputs <$> _benchExes
  ]
  where
    internalLibNames :: [String]
    internalLibNames = drvName <$> _libraries

instance Pretty TargetDerivations where
  pPrint targetDrvs = funargs (map text ("mkDerivation" : toAscList (allInputs targetDrvs))) $$ vcat
    [ text "let"
    , nest 2 $ vcat $ derivationAttr <$> targetDrvs^.libraries
    , text "in" <+> lbrace $$ nest 2 (vcat
      [ attr "libraries" $ hcat
        [ lbrace
        , text "inherit"
        , text " "
        , hcat (pPrint . packageName <$> targetDrvs^.libraries)
        , semi
        , rbrace
        ]
      , attr "exes" $ derivationAttrs $ targetDrvs^.exes
      , attr "testExes" $ derivationAttrs $ targetDrvs^.testExes
      , attr "benchExes" $ derivationAttrs $ targetDrvs^.benchExes
      , nest (-2) rbrace
      ])
    ]

derivationAttr :: Derivation -> Doc
derivationAttr drv = attr (drvName drv) $ pPrint drv
    
derivationAttrs :: [Derivation] -> Doc
derivationAttrs [] = lbrace <+> rbrace
derivationAttrs drvs = vcat
    [ lbrace
    , vcat (derivationAttr <$> drvs)
    , rbrace
    ]
    
drvName :: Derivation -> String
drvName drv = unPackageName $ packageName $ drv^.pkgid
