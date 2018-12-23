{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE BangPatterns #-}

{- |
   Maintainer:  simons@cryp.to
   Stability:   provisional
   Portability: portable
 -}

module Main ( main )  where

import Distribution.Hackage.DB.Errors
import Distribution.Hackage.DB.Path
import Distribution.Hackage.DB.Utility

import Codec.Archive.Tar as Tar
import Codec.Archive.Tar.Entry as Tar
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Catch
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Lazy as BSL
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Distribution.Types.PackageName
import Distribution.Types.Version
import GHC.Generics ( Generic )
import System.FilePath

readHackageTarball :: IO (Entries FormatError)
readHackageTarball = hackageTarball >>= readTarball

readTarball :: FilePath -> IO (Entries FormatError)
readTarball = fmap Tar.read . BSL.readFile

data Builder m a = Builder
  { insertPreferredVersions :: PackageName -> EpochTime -> BSL.ByteString -> a -> m a
  , insertCabalFile :: PackageName -> Version -> EpochTime -> BSL.ByteString -> a -> m a
  , insertMetaFile :: PackageName -> Version -> EpochTime -> BSL.ByteString -> a -> m a
  }

parseTarball :: MonadThrow m => Builder m a -> Maybe EpochTime -> Entries FormatError -> a -> m a
parseTarball b (Just et) (Next e es) !db = if entryTime e > et then return db else insertEntry b e db >>= parseTarball b (Just et) es
parseTarball b Nothing   (Next e es) !db = insertEntry b e db >>= parseTarball b Nothing es
parseTarball _ _         (Fail err)  _   = throwM err
parseTarball _ _         Done        !db = return db

insertEntry :: MonadThrow m => Builder m a -> Entry -> a -> m a
insertEntry b e db =
  case (splitDirectories (entryPath e), entryContent e) of
    ([pn,"preferred-versions"], NormalFile buf _) -> insertPreferredVersions b (mkPackageName pn) (entryTime e) buf db
    ([pn,v,file],               NormalFile buf _)
      | takeExtension file == ".cabal"            -> insertCabalFile b (mkPackageName pn) (parseText "Version" v) (entryTime e) buf db
      | takeExtension file == ".json"             -> insertMetaFile b (mkPackageName pn) (parseText "Version" v) (entryTime e) buf db
    _                                             -> throwM (UnsupportedTarEntry e)

----- Test Code

main :: IO ()
main = do
  es <- readHackageTarball
  db1 <- parseTarball hackageDbBuilder Nothing es (mempty :: HackageDB)
  let db2 = case parseTarball hackageDbBuilder Nothing es mempty of
              Left e -> error (show (e :: SomeException))
              Right db -> db
  unless (db1 == db2) $
    fail "This is not supposed to happen."

type HackageDB = Map PackageName PackageData

data PackageData = PackageData
  { versions :: !(Map Version PackageVersionData)
  , preferredVersions :: !BSS.ByteString
  }
  deriving (Show, Eq, Generic, NFData)

data PackageVersionData = PackageVersionData
  { cabalFile :: !BSS.ByteString
  , metaFile :: !BSS.ByteString
  }
  deriving (Show, Eq, Generic, NFData)

hackageDbBuilder :: Monad m => Builder m HackageDB
hackageDbBuilder = Builder
  { insertPreferredVersions = \pn _ buf -> let new     = PackageData mempty (BSL.toStrict buf)
                                               f old _ = old { preferredVersions = preferredVersions new }
                                           in return . Map.insertWith f pn new

  , insertCabalFile         = \pn v _ buf -> let f Nothing   = PackageData (Map.singleton v new) mempty
                                                 f (Just pd) = pd { versions = Map.insertWith g v new (versions pd) }
                                                 new         = PackageVersionData (BSL.toStrict buf) mempty
                                                 g old _     = old { cabalFile = cabalFile new }
                                             in return . Map.alter (Just . f) pn

  , insertMetaFile          = \pn v _ buf -> let f Nothing   = PackageData (Map.singleton v new) mempty
                                                 f (Just pd) = pd { versions = Map.insertWith g v new (versions pd) }

                                                 new         = PackageVersionData mempty (BSL.toStrict buf)
                                                 g old _     = old { metaFile = metaFile new }
                                             in return . Map.alter (Just . f) pn
  }
