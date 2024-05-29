{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Distribution.Nixpkgs.Haskell.TargetDerivations
  ( TargetDerivations, targetDerivation, derivations, libraries, exes, testExes, benchExes
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
  , _exes :: [Derivation]
  , _testExes :: [Derivation]
  , _benchExes :: [Derivation]
  }
  deriving (Show, Generic)

instance NFData TargetDerivations

makeLenses ''TargetDerivations

makeLensesFor [("_libraries", "derivations"), ("_executables", "derivations"), ("_testExecutables", "derivations"), ("_benchExecutables", "derivations")] ''TargetDerivations

targetDerivation :: Traversal' TargetDerivations Derivation
targetDerivation f (TargetDerivations {..}) = do
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

instance Pretty TargetDerivations where
  pPrint TargetDerivations {} = undefined -- TODO
