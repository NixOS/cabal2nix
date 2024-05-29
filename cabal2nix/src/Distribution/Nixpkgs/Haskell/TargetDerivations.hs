{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Distribution.Nixpkgs.Haskell.TargetDerivations
  ( TargetDerivations, targetDerivation, derivations, libraries, executables, testExecutables, benchExecutables
  , nullTargetDerivations
  )
  where

import Prelude hiding ((<>))

import Control.DeepSeq
import Control.Lens
import Distribution.Nixpkgs.Haskell.OrphanInstances ( )
import GHC.Generics ( Generic )
import Distribution.Nixpkgs.Haskell.Derivation
import Language.Nix.PrettyPrinting

-- | A represtation of Nix expressions for building individual targets within a Haskell package.

data TargetDerivations = TargetDerivations
  { _libraries :: [Derivation]
  , _executables :: [Derivation]
  , _testExecutables :: [Derivation]
  , _benchExecutables :: [Derivation]
  }
  deriving (Show, Generic)

instance NFData TargetDerivations

makeLenses ''TargetDerivations

makeLensesFor [("_libraries", "derivations"), ("_executables", "derivations"), ("_testExecutables", "derivations"), ("_benchExecutables", "derivations")] ''TargetDerivations

targetDerivation :: Traversal' TargetDerivations Derivation
targetDerivation f (TargetDerivations {..}) = do
  libs <- traverse f _libraries
  exes <- traverse f _executables
  testExes <- traverse f _testExecutables
  benchExes <- traverse f _benchExecutables
  pure TargetDerivations
    { _libraries = libs
    , _executables = exes
    , _testExecutables = testExes
    , _benchExecutables = benchExes
    }

nullTargetDerivations :: TargetDerivations 
nullTargetDerivations = TargetDerivations
  { _libraries = mempty
  , _executables = mempty
  , _testExecutables = mempty
  , _benchExecutables = mempty
  }

instance Pretty TargetDerivations where
  pPrint TargetDerivations {} = undefined -- TODO
