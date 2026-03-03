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

import Codec.Archive.Tar
import Control.Exception
import Control.Monad.Catch
import Data.ByteString as BSS
import Data.ByteString.Lazy as BSL
import Data.Map as Map
import Data.Maybe
import Data.Time.Clock
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.Text
import Distribution.Types.PackageVersionConstraint
import Distribution.Version
import GHC.Generics ( Generic )

type HackageDB = Map PackageName PackageData

data PackageData = PackageData { versions :: !(Map Version VersionData)
                               , preferredVersions :: !VersionRange
                               }
  deriving (Show, Eq, Generic)

data VersionData = VersionData { cabalFile :: !GenericPackageDescription
                               , tarballHashes :: !(Map String String)
                               , preferred :: !Bool
                               }
  deriving (Show, Eq, Generic)

readTarball :: Maybe UTCTime -> FilePath -> IO HackageDB
readTarball snapshot tarball = fmap parseDB (U.readTarball snapshot tarball)

parseTarball :: MonadThrow m => Maybe UTCTime -> Entries FormatError -> m HackageDB
parseTarball snapshot es = fmap parseDB (U.parseTarball snapshot es mempty)

parseDB :: U.HackageDB -> HackageDB
parseDB = Map.mapWithKey parsePackageData

parsePackageData :: PackageName -> U.PackageData -> PackageData
parsePackageData pn (U.PackageData pv vs) =
  mapException (\e -> HackageDBPackageName pn (e :: SomeException)) $
    PackageData (Map.mapWithKey (parseVersionData pn vr) vs) vr
  where
    PackageVersionConstraint _ vr
      | BSS.null pv = PackageVersionConstraint pn anyVersion
      | otherwise = parseBS "preferred version range" pv

parseVersionData :: PackageName -> VersionRange -> Version -> U.VersionData -> VersionData
parseVersionData pn vr v (U.VersionData cf m) =
   mapException (\e -> HackageDBPackageVersion v (e :: SomeException)) $
     VersionData gpd (parseMetaData pn v m) (v `withinRange` vr)
  where
    gpd = fromMaybe (throw (InvalidCabalFile (show (pn,v)))) $
            parseGenericPackageDescriptionMaybe cf

parseMetaData :: PackageName -> Version -> BSS.ByteString -> Map String String
parseMetaData pn v buf | BSS.null buf = Map.empty
                       | otherwise    = maybe Map.empty U.hashes targetData
  where
    targets = U.targets (U.signed (U.parseMetaData (BSL.fromStrict buf)))
    target  = "<repo>/package/" ++ display pn ++ "-" ++ display v ++ ".tar.gz"
    targetData = Map.lookup target targets
