{- |
   Maintainer:  simons@cryp.to
   Stability:   provisional
   Portability: portable
 -}

module Distribution.Hackage.DB.Unparsed
  ( HackageDB, PackageData(..), VersionData(..)
  , readTarball, parseTarball
  )
  where

import Distribution.Hackage.DB.Errors
import Distribution.Hackage.DB.Utility

import Codec.Archive.Tar as Tar
import Codec.Archive.Tar.Entry as Tar
import Control.Exception
import Data.ByteString.Lazy as BS
import Data.Map as Map
import Data.Maybe
import Data.Time.Clock
import Distribution.Package
import Distribution.Version
import System.FilePath

type HackageDB = Map PackageName PackageData

data PackageData = PackageData { preferredVersions :: ByteString
                               , versions          :: Map Version VersionData
                               }
  deriving (Show)

data VersionData = VersionData { cabalFile :: ByteString
                               , meta      :: ByteString
                               }
  deriving (Show)

readTarball :: FilePath -> Maybe UTCTime -> IO HackageDB
readTarball path snapshot = fmap (parseTarball path snapshot) (BS.readFile path)

parseTarball :: FilePath -> Maybe UTCTime -> ByteString -> HackageDB
parseTarball path snapshot buf =
  mapException (\(UnsupportedTarEntry _ e) -> UnsupportedTarEntry path e) $
    mapException (\(IncorrectTarfile _ e) -> IncorrectTarfile path e) $
      foldEntriesUntil (maybe maxBound toEpochTime snapshot) Map.empty (Tar.read buf)

foldEntriesUntil :: EpochTime -> HackageDB -> Entries FormatError -> HackageDB
foldEntriesUntil _        db  Done       = db
foldEntriesUntil _        _  (Fail err)  = throw (IncorrectTarfile "unknown" err)
foldEntriesUntil snapshot db (Next e es) | entryTime e <= snapshot = foldEntriesUntil snapshot (handleEntry db e) es
                                         | otherwise               = db

addVersion :: Version -> (VersionData -> VersionData) -> Maybe PackageData -> PackageData
addVersion v addFile Nothing   = PackageData BS.empty (Map.singleton v (addFileMaybe addFile Nothing ))
addVersion v addFile (Just pd) = pd { versions = alter (Just . addFileMaybe addFile) v (versions pd) }

addFileMaybe :: (VersionData -> VersionData) -> Maybe VersionData -> VersionData
addFileMaybe f = f . fromMaybe (VersionData BS.empty BS.empty)

handleEntry :: HackageDB -> Entry -> HackageDB
handleEntry db e =
  let (pn':ep) = splitDirectories (entryPath e)
      pn = parseText "PackageName" pn'
  in
  case (ep, entryContent e) of

    (["preferred-versions"], NormalFile buf _) -> alter (Just . addConstraint) pn db
      where
        addConstraint :: Maybe PackageData -> PackageData
        addConstraint Nothing   = PackageData buf Map.empty
        addConstraint (Just pd) = pd { preferredVersions = buf }

    ([v',file], NormalFile buf _) -> alter (Just . addVersion v addFile) pn db
      where
        v = parseText "Version" v'

        addFile :: VersionData -> VersionData
        addFile vd | file == "package.json" = vd { meta = buf }
                   | otherwise              = vd { cabalFile = buf }

    (_, Directory) -> db                -- some tarballs have these superfluous entries
    ([], NormalFile _ _) -> db
    ([], OtherEntryType _ _ _) -> db

    _ -> throw (UnsupportedTarEntry "<unknown>" e)
