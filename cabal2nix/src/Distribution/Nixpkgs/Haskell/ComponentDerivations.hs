{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Distribution.Nixpkgs.Haskell.ComponentDerivations
  ( ComponentDerivations, libraries, executables, testExecutables, benchExecutables, nullComponentDerivations )
  where

import Prelude hiding ((<>))

import Control.DeepSeq
import Control.Lens
import Data.List ( isPrefixOf )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Set.Lens
import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.Haskell.BuildInfo
import Distribution.Nixpkgs.Haskell.Derivation
import Distribution.Nixpkgs.Haskell.OrphanInstances ( )
import Distribution.Nixpkgs.Meta
import Distribution.Package
import Distribution.PackageDescription ( FlagAssignment, unFlagName, unFlagAssignment )
import GHC.Generics ( Generic )
import Language.Nix
import Language.Nix.PrettyPrinting

-- | A represtation of Nix expressions for building Haskell packages with component granularity.

