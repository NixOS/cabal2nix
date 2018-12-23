{-# LANGUAGE DeriveGeneric #-}

{- |
   Maintainer:  simons@cryp.to
   Stability:   provisional
   Portability: portable
 -}

module Distribution.Hackage.DB.Unparsed
  ( HackageDB, PackageData(..), VersionData(..)
  , readTarball, parseTarball
  , builder
  )
  where

import qualified Distribution.Hackage.DB.Builder as Build
import Distribution.Hackage.DB.Builder ( Builder(..) )
import Distribution.Hackage.DB.Utility

import Codec.Archive.Tar as Tar
import Codec.Archive.Tar.Entry as Tar
import Control.Exception
import Control.Monad.Catch
import Data.ByteString
import Data.ByteString.Lazy ( toStrict )
import Data.Map.Strict as Map
import Data.Time.Clock
import Distribution.Types.PackageName
import Distribution.Types.Version
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
readTarball snapshot tarball = Build.readTarball tarball >>= \es -> parseTarball snapshot es mempty

parseTarball :: MonadThrow m => Maybe UTCTime -> Entries FormatError -> HackageDB -> m HackageDB
parseTarball = Build.parseTarball builder . fmap toEpochTime

builder :: Applicative m => Builder m HackageDB
builder = Builder
  { insertPreferredVersions = \pn _ buf   -> let new     = PackageData (toStrict buf) mempty
                                                 f old _ = old { preferredVersions = preferredVersions new }
                                             in pure . Map.insertWith f pn new

  , insertCabalFile         = \pn v _ buf -> let f Nothing   = PackageData mempty (Map.singleton v new)
                                                 f (Just pd) = pd { versions = Map.insertWith g v new (versions pd) }
                                                 new         = VersionData (toStrict buf) mempty
                                                 g old _     = old { cabalFile = cabalFile new }
                                             in pure . Map.alter (Just . f) pn

  , insertMetaFile          = \pn v _ buf -> let f Nothing   = PackageData mempty (Map.singleton v new)
                                                 f (Just pd) = pd { versions = Map.insertWith g v new (versions pd) }

                                                 new         = VersionData mempty (toStrict buf)
                                                 g old _     = old { metaFile = metaFile new }
                                             in pure . Map.alter (Just . f) pn
  }
