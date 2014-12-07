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

-- | A 'Map' representation of the Hackage database. For sake of
-- simplicity, we use 'String' rather than 'PackageName' to represent
-- the name of a package.

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

    parsePackage :: String -> Version -> ByteString -> GenericPackageDescription
    parsePackage name version buf = case parsePackageDescription (decodeUTF8 buf) of
      ParseOk _ a     -> a
      ParseFailed err -> error ("Hackage.DB.parseHackage: cannot parse cabal file " ++ show name ++ "-" ++ display version ++ ": " ++ show err)

    decodeUTF8 :: ByteString -> String
    decodeUTF8 = toString . fromRep . BSC.unpack
