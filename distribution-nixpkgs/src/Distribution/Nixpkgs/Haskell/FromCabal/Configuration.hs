{-# LANGUAGE DeriveGeneric #-}

module Distribution.Nixpkgs.Haskell.FromCabal.Configuration
  ( Configuration(..), allPlatforms, linux, darwin, arch32, arch64
  , module Distribution.Compiler
  , module Distribution.Nixpkgs.Haskell.Constraint
  , module Distribution.Package
  , module Distribution.System
  , module Distribution.Version
  , module Language.Nix.Identifier
  ) where

import Control.DeepSeq.Generics
import Data.Map as Map
import Data.Set as Set
import Distribution.Compiler
import Distribution.Nixpkgs.Haskell.Constraint
import Distribution.Package
import Distribution.System
import Distribution.Version
import GHC.Generics ( Generic )
import Language.Nix.Identifier

data Configuration = Configuration
  {
  -- |Target architecture. Used by 'finalizePackageDescription' to
  -- choose appropriate flags and dependencies.
    platform :: Platform

  -- |Target compiler. Used by 'finalizePackageDescription' to choose
  -- appropriate flags and dependencies.
  , compilerInfo :: CompilerInfo

  -- |Core packages found on Hackageg
  , corePackages :: [PackageIdentifier]

  -- |Core packages not found on Hackage.
  , hardCorePackages :: [PackageIdentifier]

  -- |These packages replace the latest respective version during
  -- dependency resolution.
  , defaultPackageOverrides :: [Constraint]

  -- |These packages are added to the generated set, but the play no
  -- role during dependency resolution.
  , extraPackages :: [Constraint]

  -- |We know that these packages won't build, so we give them an empty
  -- meta.hydraPlatforms attribute to avoid cluttering our Hydra output with
  -- lots of failure messages.
  , dontDistributePackages :: Map PackageName (Set Platform)

  -- |This information is used by the @hackage2nix@ utility to determine the
  -- 'maintainers' for a given Haskell package.
  , packageMaintainers :: Map PackageName (Set Identifier)
  }
  deriving (Show, Generic)

instance NFData Configuration where rnf = genericRnf

allPlatforms :: Set Platform
allPlatforms = Set.fromList [ Platform I386 Linux, Platform X86_64 Linux
                            , Platform X86_64 (OtherOS "darwin")
                            ]

linux, darwin :: Set Platform
linux = Set.filter (\(Platform _ os) -> os == Linux) allPlatforms
darwin = Set.filter (\(Platform _ os) -> os == OtherOS "darwin") allPlatforms

arch32, arch64 :: Set Platform
arch32 = Set.filter (\(Platform arch _) -> arch == I386) allPlatforms
arch64 = Set.filter (\(Platform arch _) -> arch == X86_64) allPlatforms

{-# ANN module "HLint: ignore Use import/export shortcut" #-}
