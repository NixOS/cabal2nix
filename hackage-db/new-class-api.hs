{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

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
import Control.Monad.State.Strict
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Lazy as BSL
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import GHC.Generics ( Generic )
import Distribution.Types.PackageName
import Distribution.Types.Version
import System.FilePath

readHackageTarball :: IO (Entries FormatError)
readHackageTarball = hackageTarball >>= readTarball

readTarball :: FilePath -> IO (Entries FormatError)
readTarball = fmap Tar.read . BSL.readFile

class MonadThrow m => Builder m where
  consumePreferredVersions :: PackageName -> EpochTime -> BSL.ByteString -> m ()
  consumeCabalFile :: PackageName -> Version -> EpochTime -> BSL.ByteString -> m ()
  consumeMetaFile :: PackageName -> Version -> EpochTime -> BSL.ByteString -> m ()

parseTarball :: Builder m => Maybe EpochTime -> Entries FormatError -> m ()
parseTarball (Just et) (Next e es) = unless (entryTime e > et) (consumeEntry e >> parseTarball (Just et) es)
parseTarball Nothing   (Next e es) = consumeEntry e >> parseTarball Nothing es
parseTarball _         (Fail err)  = throwM err
parseTarball _         Done        = return ()

consumeEntry :: Builder m => Entry -> m ()
consumeEntry e =
  case (splitDirectories (entryPath e), entryContent e) of
    ([pn,"preferred-versions"], NormalFile buf _) -> consumePreferredVersions (mkPackageName pn) (entryTime e) buf
    ([pn,v,file],               NormalFile buf _)
      | takeExtension file == ".cabal"            -> consumeCabalFile (mkPackageName pn) (parseText "Version" v) (entryTime e) buf
      | takeExtension file == ".json"             -> consumeMetaFile (mkPackageName pn) (parseText "Version" v) (entryTime e) buf
    _                                             -> throwM (UnsupportedTarEntry e)

----- Test Code

main :: IO ()
main = do
  -- snapshot <- parseIso8601 "2018-12-20T02:09:00Z"
  -- let et <- toEpochTime snapshot
  es <- readHackageTarball

  db1 <- execStateT (parseTarball Nothing es) (mempty :: HackageDB)

  let db2 = case execStateT (parseTarball Nothing es) mempty of
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

instance MonadThrow m => Builder (StateT HackageDB m) where
  consumePreferredVersions pn _ buf = modify (Map.insertWith f pn new)
    where
      new     = PackageData mempty (BSL.toStrict buf)
      f old _ = old { preferredVersions = preferredVersions new }

  consumeCabalFile pn v _ buf = modify (Map.alter (Just . f) pn)
    where
      f Nothing   = PackageData (Map.singleton v new) mempty
      f (Just pd) = pd { versions = Map.insertWith g v new (versions pd) }

      new         = PackageVersionData (BSL.toStrict buf) mempty
      g old _     = old { cabalFile = cabalFile new }

  consumeMetaFile pn v _ buf = modify (Map.alter (Just . f) pn)
    where
      f Nothing   = PackageData (Map.singleton v new) mempty
      f (Just pd) = pd { versions = Map.insertWith g v new (versions pd) }

      new         = PackageVersionData mempty (BSL.toStrict buf)
      g old _     = old { metaFile = metaFile new }
