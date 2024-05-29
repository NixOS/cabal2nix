{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Distribution.Nixpkgs.Haskell.PackageNix 
  ( PackageNix, derivation, singleDrv, targetDrvs, nullSingleDrvPackage, nullTargetDrvsPackage
  ) where

import Control.DeepSeq
import Control.Lens
import GHC.Generics ( Generic )
import Distribution.Nixpkgs.Haskell.Derivation
import Distribution.Nixpkgs.Haskell.TargetDerivations
import Language.Nix.PrettyPrinting

data PackageNix 
  = SingleDerivation { _singleDrv :: SingleDerivation }
  | TargetDerivations { _targetDrvs :: TargetDerivations }
  deriving (Show, Generic)

instance NFData PackageNix

makeLenses ''PackageNix

derivation :: Traversal' PackageNix Derivation
derivation f (SingleDerivation {..}) = do
  singleDrv' <- singleDerivation f _singleDrv
  pure SingleDerivation { _singleDrv = singleDrv' }
derivation f (TargetDerivations {..}) = do
  targetDrvs' <- targetDerivation f _targetDrvs
  pure TargetDerivations { _targetDrvs = targetDrvs' }

instance Pretty PackageNix where
  pPrint SingleDerivation {..} = pPrint _singleDrv
  pPrint TargetDerivations {..} = pPrint _targetDrvs

nullSingleDrvPackage :: PackageNix
nullSingleDrvPackage = SingleDerivation { _singleDrv = nullSingleDerivation }

nullTargetDrvsPackage :: PackageNix
nullTargetDrvsPackage = SingleDerivation { _singleDrv = nullSingleDerivation }
