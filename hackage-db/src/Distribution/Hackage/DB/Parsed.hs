{- |
   Module      :  Distribution.Hackage.DB.Parsed
   License     :  BSD3
   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  portable

   This module provides simple access to the Hackage database by means
   of 'Map'.
 -}

module Distribution.Hackage.DB.Parsed
  ( Hackage, readHackage, readHackage', parseHackage
  , parseUnparsedHackage
  , parsePackage, parsePackage'
  )
  where

import qualified Data.ByteString.Lazy as BSC ( unpack )
import Data.ByteString.Lazy.Char8 ( ByteString )
import Data.Map
import Data.String.UTF8 ( toString, fromRep )
import Data.Version
import qualified Distribution.Hackage.DB.Unparsed as Unparsed
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse ( parsePackageDescription, ParseResult(..) )
import Distribution.Text ( display )

-- | A 'Map' representation of the Hackage database. Every package name
-- maps to a non-empty set of version, and for every version there is a
-- parsed Cabal file.

type Hackage = Map String (Map Version GenericPackageDescription)

-- | Read the Hackage database from the location determined by 'hackagePath'
-- and return a 'Map' that provides fast access to its contents.

readHackage :: IO Hackage
readHackage = fmap parseUnparsedHackage Unparsed.readHackage

-- | Read the Hackage database from the given 'FilePath' and return a
-- 'Hackage' map that provides fast access to its contents.

readHackage' :: FilePath -> IO Hackage
readHackage' = fmap parseUnparsedHackage . Unparsed.readHackage'

-- | Parse the contents of Hackage's @00-index.tar@ into a 'Hackage' map.

parseHackage :: ByteString -> Hackage
parseHackage = parseUnparsedHackage . Unparsed.parseHackage

-- | Convert an 'Unparsed.Hackage' map into a parsed 'Hackage' map.

parseUnparsedHackage :: Unparsed.Hackage -> Hackage
parseUnparsedHackage = Data.Map.mapWithKey parsePackages
  where
    parsePackages :: String -> Map Version ByteString -> Map Version GenericPackageDescription
    parsePackages name = Data.Map.mapWithKey (parsePackage name)

-- | Convenience wrapper around 'parsePackage'' to parse a single Cabal
-- file. Failure is reported with 'error'.

parsePackage :: String -> Version -> ByteString -> GenericPackageDescription
parsePackage name version buf = case parsePackage' buf of
                           Right a  -> a
                           Left err -> error $ "cannot parse cabal package " ++ show name ++ "-" ++ display version ++ ": " ++ err

-- | Parse a single Cabal file.

parsePackage' :: ByteString -> Either String GenericPackageDescription
parsePackage' buf = case parsePackageDescription (decodeUTF8 buf) of
                     ParseOk _ a     -> Right a
                     ParseFailed err -> Left (show err)
  where
    decodeUTF8 :: ByteString -> String
    decodeUTF8 = toString . fromRep . BSC.unpack
