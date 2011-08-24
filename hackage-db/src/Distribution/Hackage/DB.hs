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
import qualified Data.ByteString.Lazy.Char8 as BS
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
-- @$HOME\/.cabal\/packages\/hackage.haskell.org\/00-index.tar@ and
-- return a 'Map' that provides fast access to its contents. That @tar@
-- file is typically created by running the command @\"cabal update\"@.

readHackage :: IO Hackage
readHackage = do
  homedir <- getHomeDirectory
  readHackage' (joinPath [ homedir, ".cabal", "packages", "hackage.haskell.org", "00-index.tar" ])

-- | Read the Hackage database from the given 'FilePath' and return a
-- 'Hackage' map that provides fast access to its contents.

readHackage' :: FilePath -> IO Hackage
readHackage' = fmap parseHackage . BS.readFile

-- | Parse the contents of Hackage's @00-index.tar@ into a 'Hackage' map.

parseHackage :: ByteString -> Hackage
parseHackage = Tar.foldEntries addEntry empty error . Tar.read
  where
    addEntry :: Tar.Entry -> Hackage -> Hackage
    addEntry e db = case splitDirectories (Tar.entryPath e) of
                        path@[name,vers,_] -> case Tar.entryContent e of
                                                Tar.NormalFile buf _ -> add name vers buf db
                                                _                    -> error ("unexpected content of " ++ show path)
                        _                  -> db

    add :: String -> String -> ByteString -> Hackage -> Hackage
    add name version pkg = insertWith union name (singleton (pVersion version) (pPackage name pkg))

    pPackage :: String -> ByteString -> GenericPackageDescription
    pPackage name buf = case parsePackageDescription (BS.unpack buf) of
                          ParseOk _ a     -> a
                          ParseFailed err -> error ("cannot parse cabal file " ++ show name ++ ": " ++ show err)

    pVersion :: String -> Version
    pVersion str = fromMaybe (error $ "cannot parse version " ++ show str) (simpleParse str)
