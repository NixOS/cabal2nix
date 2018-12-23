{-# LANGUAGE BangPatterns #-}

{- |
   Maintainer:  simons@cryp.to
   Stability:   provisional
   Portability: portable
 -}

module Distribution.Hackage.DB.Builder
  ( readTarball, parseTarball
  , Builder(..)
  )
  where

import Distribution.Hackage.DB.Errors
import Distribution.Hackage.DB.Path
import Distribution.Hackage.DB.Utility

import Codec.Archive.Tar as Tar
import Codec.Archive.Tar.Entry as Tar
import Control.Monad.Catch
import qualified Data.ByteString.Lazy as BSL
import Distribution.Types.PackageName
import Distribution.Types.Version
import System.FilePath

readTarball :: FilePath -> IO (Entries FormatError)
readTarball = fmap Tar.read . BSL.readFile

data Builder m a = Builder
  { insertPreferredVersions :: PackageName -> EpochTime -> BSL.ByteString -> a -> m a
  , insertCabalFile :: PackageName -> Version -> EpochTime -> BSL.ByteString -> a -> m a
  , insertMetaFile :: PackageName -> Version -> EpochTime -> BSL.ByteString -> a -> m a
  }

{-# INLINABLE parseTarball #-}
parseTarball :: MonadThrow m => Builder m a -> Maybe EpochTime -> Entries FormatError -> a -> m a
parseTarball b (Just et) (Next e es) !db = if entryTime e > et then return db else insertEntry b e db >>= parseTarball b (Just et) es
parseTarball b Nothing   (Next e es) !db = insertEntry b e db >>= parseTarball b Nothing es
parseTarball _ _         (Fail err)  _   = throwM err
parseTarball _ _         Done        !db = return db

{-# INLINABLE insertEntry #-}
insertEntry :: MonadThrow m => Builder m a -> Entry -> a -> m a
insertEntry b e db =
  case (splitDirectories (entryPath e), entryContent e) of
    ([pn,"preferred-versions"], NormalFile buf _) -> insertPreferredVersions b (mkPackageName pn) (entryTime e) buf db
    ([pn,v,file],               NormalFile buf _)
      | takeExtension file == ".cabal"            -> insertCabalFile b (mkPackageName pn) (parseText "Version" v) (entryTime e) buf db
      | takeExtension file == ".json"             -> insertMetaFile b (mkPackageName pn) (parseText "Version" v) (entryTime e) buf db
    _                                             -> throwM (UnsupportedTarEntry e)
