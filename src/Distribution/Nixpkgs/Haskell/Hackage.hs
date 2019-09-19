module Distribution.Nixpkgs.Haskell.Hackage
  ( HackageDB, PackageData, VersionData(..), NixSha256
  , hackageTarball, readTarball, parsePackageData
  )
  where

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)
import Data.Map as Map
import qualified Distribution.Hackage.DB.Parsed as P
import Distribution.Hackage.DB.Path
import qualified Distribution.Hackage.DB.Unparsed as U
import Distribution.Nixpkgs.Hashes
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version
import OpenSSL.Digest
import Data.Time

type HackageDB = Map PackageName PackageData

type PackageData = Map Version VersionData

type NixSha256 = String

data VersionData = VersionData
  { cabalFilesWithHashes :: !(NonEmpty (NixSha256, GenericPackageDescription))
  , tarballSha256 :: !(Maybe NixSha256)
  }
  deriving (Show)

readTarball :: Maybe UTCTime -> FilePath -> IO HackageDB
readTarball ts p = do
  dbu <- U.readTarball ts p
  let dbp = P.parseDB dbu
  return (mapWithKey (parsePackageData dbu) dbp)

parsePackageData :: U.HackageDB -> PackageName -> P.PackageData -> PackageData
parsePackageData dbu pn = mapWithKey (parseVersionData (dbu ! pn))

parseVersionData :: U.PackageData -> Version -> P.VersionData -> VersionData
parseVersionData pdu v vd = VersionData
                            { cabalFilesWithHashes = cfsWithHashes
                            , tarballSha256 = Map.lookup "sha256" (P.tarballHashes vd)
                            }
  where
    cfsWithHashes = NE.zip sha256s (P.cabalFileRevisions vd)
    sha256s = NE.map (\file -> printSHA256 (digest (digestByName "sha256") file))
              (NE.fromList $ U.cabalFileRevisions (U.versions pdu ! v))
