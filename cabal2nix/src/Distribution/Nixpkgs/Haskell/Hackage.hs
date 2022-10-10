module Distribution.Nixpkgs.Haskell.Hackage
  ( HackageDB, PackageData, VersionData(..)
  , hackageTarball, readTarball, parsePackageData
  )
  where

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

data VersionData = VersionData
  { cabalFile :: !GenericPackageDescription
  , cabalFileSha256 :: !String
  , tarballSha256 :: !(Maybe String)
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
                            { cabalFile = P.cabalFile vd
                            , cabalFileSha256 = printSHA256 (digest (digestByName "sha256") file)
                            , tarballSha256 = Map.lookup "sha256" (P.tarballHashes vd)
                            }
  where
    file = U.cabalFile (U.versions pdu ! v)
