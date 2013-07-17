{-# LANGUAGE CPP #-}
{- |
   Module      :  Distribution.Hackage.DB
   License     :  BSD3

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  portable

   This module provides simple access to the Hackage database by means
   of 'Map'. Note that once the database has been parsed, it can be
   accessed quickly, but the inital cost of reading @00-index.tar@ is
   fairly high.
 -}

module Distribution.Hackage.DB
  ( Hackage, readHackage, readHackage', parseHackage
  , module Data.Map
  , module Data.Version
  , module Distribution.Package
  , module Distribution.PackageDescription
  )
  where

import qualified Codec.Archive.Tar as Tar
import Data.ByteString.Lazy.Char8 ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.ByteString.Lazy as BSC
import Data.String.UTF8 ( toString, fromRep )
import Data.Map
import Data.Maybe
import Data.Version
import System.Directory
import System.FilePath
import Distribution.Package
import Distribution.Text
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse ( parsePackageDescription, ParseResult(..) )

-- | A 'Map' representation of the Hackage database. For sake of
-- simplicity, we use 'String' rather than 'PackageName' to represent
-- the name of a package.

type Hackage = Map String (Map Version GenericPackageDescription)

-- | Read the Hackage database from
-- @$HOME\/@/<package database path>/@\/hackage.haskell.org\/00-index.tar@ and
-- return a 'Map' that provides fast access to its contents. That @tar@
-- file is typically created by running the command @\"cabal update\"@.

readHackage :: IO Hackage
readHackage = do
  homedir <- getHomeDirectory
  readHackage' (joinPath [ homedir,
#ifdef IS_DARWIN
    "Library", "Haskell", "repo-cache"
#else
    ".cabal", "packages"
#endif
    , "hackage.haskell.org", "00-index.tar" ])

-- | Read the Hackage database from the given 'FilePath' and return a
-- 'Hackage' map that provides fast access to its contents.

readHackage' :: FilePath -> IO Hackage
readHackage' = fmap parseHackage . BS8.readFile

-- | Parse the contents of Hackage's @00-index.tar@ into a 'Hackage' map.

parseHackage :: ByteString -> Hackage
parseHackage = Tar.foldEntries addEntry empty (error . show) . Tar.read
  where
    decodeUTF8 :: ByteString -> String
    decodeUTF8 = toString . fromRep . BSC.unpack

    addEntry :: Tar.Entry -> Hackage -> Hackage
    addEntry e db = case splitDirectories (Tar.entryPath e) of
                        [".",".","@LongLink"] -> db
                        path@[name,vers,_] -> case Tar.entryContent e of
                                                Tar.NormalFile buf _ -> add name vers buf db
                                                _                    -> error ("Hackage.DB.parseHackage: unexpected content type for " ++ show path)
                        _                  -> db

    add :: String -> String -> ByteString -> Hackage -> Hackage
    add name version pkg = insertWith union name (singleton (pVersion version) (pPackage name pkg))

    pPackage :: String -> ByteString -> GenericPackageDescription
    pPackage name buf = case parsePackageDescription (decodeUTF8 buf) of
                          ParseOk _ a     -> a
                          ParseFailed err -> error ("Hackage.DB.parseHackage: cannot parse cabal file " ++ show name ++ ": " ++ show err)

    pVersion :: String -> Version
    pVersion str = fromMaybe (error $ "Hackage.DB.parseHackage: cannot parse version " ++ show str) (simpleParse str)
