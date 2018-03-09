{-# LANGUAGE DeriveGeneric #-}

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
import Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.UTF8 as BS
import Data.Map as Map
import Data.Maybe
import Data.Time.Clock
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.Text
import Distribution.Version
import GHC.Generics ( Generic )

type HackageDB = Map PackageName PackageData

type PackageData = Map Version VersionData

data VersionData = VersionData { cabalFile :: !GenericPackageDescription
                               , tarballHashes :: !(Map String String)
                               }
  deriving (Show, Eq, Generic)

readTarball :: Maybe UTCTime -> FilePath -> IO HackageDB
readTarball snapshot path = fmap (parseTarball snapshot path) (BS.readFile path)

parseTarball :: Maybe UTCTime -> FilePath -> ByteString -> HackageDB
parseTarball snapshot path buf = parseDB (U.parseTarball snapshot path buf)

parseDB :: U.HackageDB -> HackageDB
parseDB = Map.mapWithKey parsePackageData

parsePackageData :: PackageName -> U.PackageData -> PackageData
parsePackageData pn (U.PackageData pv vs') =
  mapException (\e -> HackageDBPackageName pn (e :: SomeException)) $
    Map.mapWithKey (parseVersionData pn) $
      Map.filterWithKey (\v _ -> v `withinRange` vr) vs'
  where
    Dependency _ vr | BS.null pv = Dependency pn anyVersion
                    | otherwise  = parseText "preferred version range" (toString pv)

parseVersionData :: PackageName -> Version -> U.VersionData -> VersionData
parseVersionData pn v (U.VersionData cf m) =
   mapException (\e -> HackageDBPackageVersion v (e :: SomeException)) $
     VersionData gpd (parseMetaData pn v m)
  where
    gpd = fromMaybe (throw (InvalidCabalFile (show (pn,v)))) $
            parseGenericPackageDescriptionMaybe (toStrict cf)

parseMetaData :: PackageName -> Version -> ByteString -> Map String String
parseMetaData pn v buf | BS.null buf = Map.empty
                       | otherwise   = maybe Map.empty U.hashes targetData
  where
    targets = U.targets (U.signed (U.parseMetaData buf))
    target  = "<repo>/package/" ++ display pn ++ "-" ++ display v ++ ".tar.gz"
    targetData = Map.lookup target targets
