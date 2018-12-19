{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
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

data Builder m = Builder
  { consumePreferredVersions :: PackageName -> EpochTime -> BSL.ByteString -> m ()
  , consumeCabalFile :: PackageName -> Version -> EpochTime -> BSL.ByteString -> m ()
  , consumeMetaFile :: PackageName -> Version -> EpochTime -> BSL.ByteString -> m ()
  , consumeError :: SomeException -> m ()
  }

parseTarball :: Applicative m => Builder m -> Maybe EpochTime -> Entries FormatError -> m ()
parseTarball b (Just et) (Next e es) = unless (entryTime e > et) (consumeEntry b e *> parseTarball b (Just et) es)
parseTarball b Nothing   (Next e es) = consumeEntry b e *> parseTarball b Nothing es
parseTarball b _         (Fail err)  = consumeError b (toException err)
parseTarball _ _         Done        = pure ()

consumeEntry :: Builder m -> Entry -> m ()
consumeEntry b e =
  case (splitDirectories (entryPath e), entryContent e) of
    ([pn,"preferred-versions"], NormalFile buf _) -> consumePreferredVersions b (mkPackageName pn) (entryTime e) buf
    ([pn,v,file],               NormalFile buf _)
      | takeExtension file == ".cabal"            -> consumeCabalFile b (mkPackageName pn) (parseText "Version" v) (entryTime e) buf
      | takeExtension file == ".json"             -> consumeMetaFile b (mkPackageName pn) (parseText "Version" v) (entryTime e) buf
    _                                             -> consumeError b (toException (UnsupportedTarEntry e))

----- Test Code

main :: IO ()
main = do
  es <- readHackageTarball
  db1 <- execStateT (parseTarball hackageDbBuilder Nothing es) mempty
  let db2 = execState (parseTarball hackageDbBuilder Nothing es) mempty
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

hackageDbBuilder :: MonadState HackageDB m => Builder m
hackageDbBuilder = Builder
  { consumePreferredVersions = \pn _ buf -> let new     = PackageData mempty (BSL.toStrict buf)
                                                f old _ = old { preferredVersions = preferredVersions new }
                                            in modify (Map.insertWith f pn new)

  , consumeCabalFile         = \pn v _ buf -> let f Nothing   = PackageData (Map.singleton v new) mempty
                                                  f (Just pd) = pd { versions = Map.insertWith g v new (versions pd) }
                                                  new         = PackageVersionData (BSL.toStrict buf) mempty
                                                  g old _     = old { cabalFile = cabalFile new }
                                              in modify (Map.alter (Just . f) pn)

  , consumeMetaFile          = \pn v _ buf -> let f Nothing   = PackageData (Map.singleton v new) mempty
                                                  f (Just pd) = pd { versions = Map.insertWith g v new (versions pd) }

                                                  new         = PackageVersionData mempty (BSL.toStrict buf)
                                                  g old _     = old { metaFile = metaFile new }
                                              in modify (Map.alter (Just . f) pn)
  , consumeError             = fail . show
  }
