{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Distribution.Nixpkgs.Haskell.PackageNix 
  ( PackageNix, derivation, singleDrv, targetDrvs, nullSingleDrvPackage, nullTargetDrvsPackage
  , allDependencies, allExtraFunctionArgs
  ) where

import Control.DeepSeq
import Control.Lens
import Data.Set ( Set )
import Distribution.Nixpkgs.Haskell.BuildInfo
import Distribution.Nixpkgs.Haskell.Derivation
import Distribution.Nixpkgs.Haskell.TargetDerivations
import GHC.Generics ( Generic )
import Language.Nix
import Language.Nix.PrettyPrinting

data PackageNix 
  = SingleDerivation { _singleDrv :: SingleDerivation }
  | TargetDerivations { _targetDrvs :: TargetDerivations }
  deriving (Show, Generic)

instance NFData PackageNix

makeLenses ''PackageNix

derivation :: Traversal' PackageNix Derivation
derivation f SingleDerivation {..} = do
  singleDrv' <- singleDerivation f _singleDrv
  pure SingleDerivation { _singleDrv = singleDrv' }
derivation f TargetDerivations {..} = do
  targetDrvs' <- targetDerivations f _targetDrvs
  pure TargetDerivations { _targetDrvs = targetDrvs' }

allDependencies :: Traversal' PackageNix BuildInfo
allDependencies f SingleDerivation {..} = do
  singleDrv' <- (singleDerivation . dependencies) f _singleDrv
  pure SingleDerivation { _singleDrv = singleDrv' }
allDependencies f TargetDerivations {..} = do
  targetDrvs' <- (targetDerivations . dependencies) f _targetDrvs
  pure TargetDerivations { _targetDrvs = targetDrvs' }

allExtraFunctionArgs :: Traversal' PackageNix (Set Binding)
allExtraFunctionArgs f SingleDerivation {..} = do
  singleDrv' <- (singleDerivation . extraFunctionArgs) f _singleDrv
  pure SingleDerivation { _singleDrv = singleDrv' }
allExtraFunctionArgs f TargetDerivations {..} = do
  targetDrvs' <- (targetDerivations . extraFunctionArgs) f _targetDrvs
  pure TargetDerivations { _targetDrvs = targetDrvs' }

instance Pretty PackageNix where
  pPrint SingleDerivation {..} = pPrint _singleDrv
  pPrint TargetDerivations {..} = pPrint _targetDrvs

nullSingleDrvPackage :: PackageNix
nullSingleDrvPackage = SingleDerivation { _singleDrv = nullSingleDerivation }

nullTargetDrvsPackage :: PackageNix
nullTargetDrvsPackage = TargetDerivations { _targetDrvs = nullTargetDerivations }
