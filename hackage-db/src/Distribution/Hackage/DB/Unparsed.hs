{-# LANGUAGE DeriveGeneric #-}

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
import Data.ByteString.Lazy as BS ( ByteString, empty, readFile )
import Data.Map as Map
import Data.Time.Clock
import Distribution.Package
import Distribution.Version
import GHC.Generics ( Generic )
import System.FilePath

type HackageDB = Map PackageName PackageData

data PackageData = PackageData { preferredVersions :: ByteString
                               , versions          :: Map Version VersionData
                               }
  deriving (Show, Eq, Generic)

data VersionData = VersionData { cabalFile :: ByteString
                               , metaFile  :: ByteString
                               }
  deriving (Show, Eq, Generic)

readTarball :: Maybe UTCTime -> FilePath -> IO HackageDB
readTarball snapshot path = fmap (parseTarball snapshot path) (BS.readFile path)

parseTarball :: Maybe UTCTime -> FilePath -> ByteString -> HackageDB
parseTarball snapshot path buf =
  mapException (\e -> HackageDBTarball path (e :: SomeException)) $
    foldEntriesUntil (maybe maxBound toEpochTime snapshot) Map.empty (Tar.read buf)

foldEntriesUntil :: EpochTime -> HackageDB -> Entries FormatError -> HackageDB
foldEntriesUntil _        db  Done       = db
foldEntriesUntil _        _  (Fail err)  = throw (IncorrectTarfile err)
foldEntriesUntil snapshot db (Next e es) | entryTime e <= snapshot = foldEntriesUntil snapshot (handleEntry db e) es
                                         | otherwise               = db

handleEntry :: HackageDB -> Entry -> HackageDB
handleEntry db e =
  let (pn':ep) = splitDirectories (entryPath e)
      pn = parseText "PackageName" pn'
  in
  case (ep, entryContent e) of

    (["preferred-versions"], NormalFile buf _) -> insertWith setConstraint pn (PackageData buf Map.empty) db

    ([v',file], NormalFile buf _) -> let v = parseText "Version" v' in
          if file == pn' <.> "cabal" then insertVersionData setCabalFile pn v (VersionData buf BS.empty) db else
          if file == "package.json" then insertVersionData setMetaFile pn v (VersionData BS.empty buf) db else
          throw (UnsupportedTarEntry e)

    (_, Directory) -> db                -- some tarballs have these superfluous entries
    ([], NormalFile {}) -> db
    ([], OtherEntryType {}) -> db

    _ -> throw (UnsupportedTarEntry e)

setConstraint :: PackageData -> PackageData -> PackageData
setConstraint new old = old { preferredVersions = preferredVersions new }

insertVersionData :: (VersionData -> VersionData -> VersionData)
                   -> PackageName -> Version -> VersionData
                   -> HackageDB -> HackageDB
insertVersionData setFile pn v vd = insertWith mergeVersionData pn pd
  where
    pd = PackageData BS.empty (Map.singleton v vd)
    mergeVersionData _ old = old { versions = insertWith setFile v vd (versions old) }

setCabalFile :: VersionData -> VersionData -> VersionData
setCabalFile new old = old { cabalFile = cabalFile new }

setMetaFile :: VersionData -> VersionData -> VersionData
setMetaFile new old = old { metaFile = metaFile new }
