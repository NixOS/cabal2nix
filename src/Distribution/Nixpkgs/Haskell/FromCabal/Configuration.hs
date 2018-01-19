{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Distribution.Nixpkgs.Haskell.FromCabal.Configuration ( Configuration(..), readConfiguration, assertConsistency ) where

import Control.DeepSeq
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Map as Map
import Data.Set as Set
import Data.Text as T
import Data.Yaml
import Distribution.Compiler
import Distribution.Nixpkgs.Haskell.Constraint
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

  -- |We know that these packages won't build, so we give them an empty
  -- meta.hydraPlatforms attribute to avoid cluttering our Hydra output with
  -- lots of failure messages.
  , dontDistributePackages :: Map PackageName (Set Platform)

  -- |This information is used by the @hackage2nix@ utility to determine the
  -- 'maintainers' for a given Haskell package.
  , packageMaintainers :: Map Identifier (Set PackageName)
  }
  deriving (Show, Generic)

instance NFData Configuration

instance FromJSON Configuration where
  parseJSON (Object o) = Configuration
        <$> o .:? "compiler" .!= unknownCompilerInfo buildCompilerId NoAbiTag
        <*> o .:? "core-packages" .!= mempty
        <*> o .:? "default-package-overrides" .!= mempty
        <*> o .:? "extra-packages" .!= mempty
        <*> o .:? "dont-distribute-packages" .!= mempty
        <*> o .:? "package-maintainers" .!= mempty
  parseJSON _ = error "invalid Configuration"

instance FromJSON Identifier where
  parseJSON (String s) = pure (review ident (T.unpack s))
  parseJSON s = fail ("parseJSON: " ++ show s ++ " is not a valid Nix identifier")

#if MIN_VERSION_aeson(1,0,0)

instance FromJSONKey Identifier where
  fromJSONKey = FromJSONKeyText parseKey

instance FromJSONKey PackageName where
  fromJSONKey = FromJSONKeyText parseKey

#elif MIN_VERSION_aeson(0,11,0)

instance (FromJSON v) => FromJSON (Map Identifier v) where
  parseJSON = fmap (Map.mapKeys parseKey) . parseJSON

instance (FromJSON v) => FromJSON (Map PackageName v) where
  parseJSON = fmap (Map.mapKeys parseKey) . parseJSON

#else

instance (Ord k, FromJSON k, FromJSON v) => FromJSON (Map k v) where
  parseJSON  = fmap (Map.mapKeys parseKey) . parseJSON

#endif

parseKey :: FromJSON k => Text -> k
parseKey s = either error id (parseEither parseJSON (String s))

readConfiguration :: FilePath -> IO Configuration
readConfiguration path =
  decodeFile path >>= maybe (fail ("invalid config file at " ++ show path)) assertConsistency

assertConsistency :: Monad m => Configuration -> m Configuration
assertConsistency cfg@Configuration {..} = do
  let report msg = fail ("*** configuration error: " ++ msg)

      maintainedPackages = Set.unions (Map.elems packageMaintainers)
      disabledPackages = Map.keysSet dontDistributePackages
      disabledMaintainedPackages = maintainedPackages `Set.intersection` disabledPackages
  unless (Set.null disabledMaintainedPackages) $
    report ("disabled packages that have a maintainer: " ++ show disabledMaintainedPackages)

  return cfg
