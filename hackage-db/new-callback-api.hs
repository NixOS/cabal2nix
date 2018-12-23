{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

{- |
   Maintainer:  simons@cryp.to
   Stability:   provisional
   Portability: portable
 -}

module Main ( main )  where

import Distribution.Hackage.DB.Builder

import Control.DeepSeq
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Lazy as BSL
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Distribution.Types.PackageName
import Distribution.Types.Version
import GHC.Generics ( Generic )

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
