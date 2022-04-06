{-# LANGUAGE DeriveGeneric #-}

{- |
   Maintainer:  simons@cryp.to
   Stability:   provisional
   Portability: portable

   Types and class instances for parsing the @package.json@ files found in a
   @01-index.tar@ tarball from Hackage with "Data.Aeson". These parsers mirror
   the exact structure of those files, but only a small part of it is actually
   of interest to anyone other than implementors of @cabal-install@. Everyone
   else will most likely prefer the functions oferred by
   "Distribution.Hackage.DB.Parsed".
 -}

module Distribution.Hackage.DB.MetaData where

import Distribution.Hackage.DB.Errors

import Control.Exception
import Data.Aeson
import Data.ByteString.Lazy.UTF8 as BS
import Data.Map as Map
import GHC.Generics ( Generic )

-- | Parse the @package.json@ file found in a @01-index.tar@ tarball from
-- Hackage with "Data.Aeson". This function is a convenience wrapper around
-- 'eitherDecode' that throws an 'InvalidMetaFile' exception to signal failure.
--
-- >>> :set -XOverloadedStrings
-- >>> parseMetaData "{\"signatures\":[],\"signed\":{\"_type\":\"Targets\",\"expires\":null,\"targets\":{\"<repo>/package/jailbreak-cabal-1.3.2.tar.gz\":{\"hashes\":{\"md5\":\"ba42b3e68323ccbeb3ac900cd68f9e90\",\"sha256\":\"212a8bbc3dfc748c4063282414a2726709d651322f3984c9989179d2352950f4\"},\"length\":2269}},\"version\":0}}"
-- MetaData {signed = SignedMetaData {version = 0, expires = Nothing, _type = "Targets", targets = fromList [("<repo>/package/jailbreak-cabal-1.3.2.tar.gz",TargetData {length = 2269, hashes = fromList [("md5","ba42b3e68323ccbeb3ac900cd68f9e90"),("sha256","212a8bbc3dfc748c4063282414a2726709d651322f3984c9989179d2352950f4")]})]}, signatures = []}

parseMetaData :: ByteString -> MetaData
parseMetaData = either (throw . InvalidMetaFile) id . eitherDecode

data MetaData = MetaData { signed :: SignedMetaData
                         , signatures :: [Signature]
                         }
  deriving (Show, Generic)

instance FromJSON MetaData

data SignedMetaData = SignedMetaData { version :: Int
                                     , expires :: Maybe String
                                     , _type   :: String
                                     , targets :: Map String TargetData
                                     }
  deriving (Show, Generic)

instance FromJSON SignedMetaData

data TargetData = TargetData { length :: Int
                             , hashes :: Map String String
                             }
  deriving (Show, Generic)

instance FromJSON TargetData

data Signature = Signature { keyid :: String
                           , method :: String
                           , sig :: String }
  deriving (Show, Generic)

instance FromJSON Signature
