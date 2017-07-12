{- |
   Maintainer:  simons@cryp.to
   Stability:   provisional
   Portability: portable
 -}

module Distribution.Hackage.DB.Parsed where

import Distribution.Hackage.DB.Errors
import qualified Distribution.Hackage.DB.MetaData as U
import qualified Distribution.Hackage.DB.Unparsed as U
import Distribution.Hackage.DB.Utility

import Control.Exception
import Data.ByteString.Lazy.UTF8 as BS
import Data.Map as Map
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Version

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
parsePackageData pn (U.PackageData pv vs) =
  mapException (\e -> HackageDBPackageName pn (e :: SomeException)) $
    PackageData vr (Map.mapWithKey (parseVersionData pn) vs)
      where
        Dependency _ vr = parseText "Dependency" (toString pv)

parseVersionData :: PackageName -> Version -> U.VersionData -> VersionData
parseVersionData pn v (U.VersionData cf m) =
  mapException (\e -> HackageDBPackageVersion v (e :: SomeException)) $
    VersionData gpd md
  where
    gpd = case parsePackageDescription (toString cf) of
            ParseOk _ a     -> a
            ParseFailed msg -> throw (InvalidCabalFile (show msg))

    md = U.parseMetaData m

type MetaData = U.MetaData
