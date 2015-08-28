module Distribution.Nixpkgs.Haskell.FromCabal.Configuration
  ( Configuration(..)
  , module Distribution.Package
  , module Distribution.System
  , module Distribution.Compiler
  , module Distribution.Nixpkgs.Haskell.Constraint
  , module Distribution.Version
  ) where

import Data.Set
import Distribution.Package
import Distribution.System
import Distribution.Compiler
import Distribution.Nixpkgs.Haskell.Constraint
import Distribution.Version

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
  , dontDistributePackages :: Set PackageName
  }
  deriving (Show)
