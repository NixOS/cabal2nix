{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Distribution.Nixpkgs.Haskell.FromCabal.Configuration
  ( Configuration(..), readConfiguration, assertConsistency
  )
  where

import Prelude hiding ( fail )

import Control.DeepSeq
import Control.Exception ( throwIO )
import Control.Lens
import Control.Monad hiding ( fail )
import Control.Monad.Fail
import Data.Aeson
import Data.Map as Map
import Data.Set as Set
import Data.Text as T
import Data.Yaml
import Distribution.Compiler
import Distribution.Nixpkgs.Haskell.Constraint
import Distribution.Nixpkgs.Meta
import Distribution.Package
import Distribution.System
import GHC.Generics ( Generic )
import Language.Nix.Identifier

data Configuration = Configuration
  {
  -- |Target compiler. Used by 'finalizePackageDescription' to choose
  -- appropriate flags and dependencies.
    compilerInfo :: CompilerInfo

  -- |Compiler core packages that are also found on Hackage.
  , corePackages :: Set PackageIdentifier

  -- |These packages replace the latest respective version during
  -- dependency resolution.
  , defaultPackageOverrides :: [Constraint]

  -- |These packages are added to the generated set, but the play no
  -- role during dependency resolution.
  , extraPackages :: [Constraint]

  -- |This information is used by the @hackage2nix@ utility to determine the
  -- 'maintainers' for a given Haskell package.
  , packageMaintainers :: Map Identifier (Set PackageName)

  -- |These packages (necessarily) only support a certain list of platforms.
  , supportedPlatforms :: Map PackageName (Set NixpkgsPlatform)

  -- |These packages (by design) don't support certain platforms.
  , unsupportedPlatforms :: Map PackageName (Set NixpkgsPlatform)

  -- |These packages cannot be distributed by Hydra, i.e. because they have an
  -- unfree license or depend on other tools that cannot be distributed for
  -- some reason.
  , dontDistributePackages :: Set PackageName

  -- |We know that these packages won't compile, so we mark them as broken and
  -- also disable their meta.hydraPlatforms attribute to avoid cluttering our
  -- Hydra job with lots of failure messages.
  , brokenPackages :: [Constraint]
  }
  deriving (Show, Generic)

instance NFData Configuration

instance Semigroup Configuration where
  l <> r = Configuration { compilerInfo = compilerInfo l
                         , corePackages = corePackages l <> corePackages r
                         , defaultPackageOverrides = defaultPackageOverrides l <> defaultPackageOverrides r
                         , extraPackages = extraPackages l <> extraPackages r
                         , packageMaintainers = packageMaintainers l <> packageMaintainers r
                         , supportedPlatforms = supportedPlatforms l <> supportedPlatforms r
                         , unsupportedPlatforms = unsupportedPlatforms l <> unsupportedPlatforms r
                         , dontDistributePackages = dontDistributePackages l <> dontDistributePackages r
                         , brokenPackages = brokenPackages l <> brokenPackages r
                         }

instance FromJSON Configuration where
  parseJSON (Object o) = Configuration
        <$> o .:? "compiler" .!= unknownCompilerInfo buildCompilerId NoAbiTag
        <*> o .:? "core-packages" .!= mempty
        <*> o .:? "default-package-overrides" .!= mempty
        <*> o .:? "extra-packages" .!= mempty
        <*> o .:? "package-maintainers" .!= mempty
        <*> o .:? "supported-platforms" .!= mempty
        <*> o .:? "unsupported-platforms" .!= mempty
        <*> o .:? "dont-distribute-packages" .!= mempty
        <*> o .:? "broken-packages" .!= mempty
  parseJSON _ = error "invalid Configuration"

instance FromJSON Identifier where
  parseJSON (String s) = pure (review ident (T.unpack s))
  parseJSON s = fail ("parseJSON: " ++ show s ++ " is not a valid Nix identifier")

instance FromJSONKey Identifier where
  fromJSONKey = FromJSONKeyText parseKey

instance FromJSONKey PackageName where
  fromJSONKey = FromJSONKeyText parseKey

parseKey :: FromJSON k => Text -> k
parseKey s = either error id (parseEither parseJSON (String s))

readConfiguration :: FilePath -> IO Configuration
readConfiguration path = decodeFileEither path >>= either throwIO assertConsistency

assertConsistency :: MonadFail m => Configuration -> m Configuration
assertConsistency cfg@Configuration {..} = do
  let report msg = fail ("*** configuration error: " ++ msg)
      maintainedPackages = Set.unions (Map.elems packageMaintainers)
      disabledPackages = dontDistributePackages `Set.union` Set.fromList (constraintPkgName <$> brokenPackages)
      disabledMaintainedPackages = maintainedPackages `Set.intersection` disabledPackages
  unless (Set.null disabledMaintainedPackages) $
    report ("disabled packages that have a maintainer: " ++ show disabledMaintainedPackages)

  return cfg
