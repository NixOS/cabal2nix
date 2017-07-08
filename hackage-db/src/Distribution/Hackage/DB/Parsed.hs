{-# LANGUAGE DeriveGeneric #-}

{- |
   Maintainer:  simons@cryp.to
   Stability:   provisional
   Portability: portable
 -}

module Distribution.Hackage.DB.Parsed where

import qualified Distribution.Hackage.DB.Unparsed as U
import Distribution.Hackage.DB.Utility

import Data.Aeson
import Data.ByteString.Lazy.UTF8 as BS
import Data.Map as Map
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Version
import GHC.Generics ( Generic )

type HackageDB = Map PackageName PackageData

data PackageData = PackageData { preferredVersions :: VersionRange
                               , versions          :: Map Version VersionData
                               }
  deriving (Show)

data VersionData = VersionData { cabalFile :: GenericPackageDescription
                               , meta      :: MetaData
                               }
  deriving (Show)

parseDB :: U.HackageDB -> HackageDB
parseDB = Map.mapWithKey parsePackageData

parsePackageData :: PackageName -> U.PackageData -> PackageData
parsePackageData pn (U.PackageData pv vs) = PackageData vr (Map.mapWithKey (parseVersionData pn) vs)
  where
    Dependency _ vr = parseText "Dependency" (toString pv)

parseVersionData :: PackageName -> Version -> U.VersionData -> VersionData
parseVersionData pn v (U.VersionData cf m) = VersionData gpd md
  where
    gpd = case parsePackageDescription (toString cf) of
            ParseOk _ a     -> a
            ParseFailed msg -> error (show msg) -- TODO: define proper exception

    md = case eitherDecode m of
           Left msg -> error msg  -- TODO: create proper exception type
           Right md' -> md'

data MetaData = MetaData { signed :: SignedMetaData
                         , signatures :: [String]
                         }
  deriving (Show, Generic)

instance FromJSON MetaData

data SignedMetaData = SignedMetaData { version :: Int
                                     , expires :: Maybe String
                                     , _type   :: String
                                     , targets :: Map String TargetData
                                     }
  deriving (Show, Generic)

instance FromJSON SignedMetaData

data TargetData = TargetData { length :: Int
                             , hashes :: Map String String
                             }
  deriving (Show, Generic)

instance FromJSON TargetData
