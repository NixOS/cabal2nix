{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Cabal2Nix.HackageGit ( Hackage, readHackage, module Data.Map )  where

import Control.DeepSeq.Generics
import GHC.Generics ( Generic )
import Control.Monad
import Data.Aeson
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS8 ( readFile )
import Data.ByteString.Lazy.Char8 ( fromStrict )
import Data.Digest.Pure.SHA ( sha256, showDigest )
import Data.Map
import Data.Maybe ( fromMaybe )
import Data.String.UTF8 ( toString, fromRep )
import Data.Version
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse ( parsePackageDescription, ParseResult(..) )
import Distribution.Text ( simpleParse, display )
import System.Directory
import System.FilePath
import Control.Monad.Par.Combinator
import Control.Monad.Par.IO
import Control.Monad.Trans ( liftIO )

-- | A 'Map' representation of the Hackage database. Every package name
-- maps to a non-empty set of version, and for every version there is a
-- Cabal file stored as a (lazy) 'ByteString'.

type Hackage = Map String (Map Version GenericPackageDescription)

getSubDirs :: FilePath -> IO [FilePath]
getSubDirs path = do
  let isDirectory p = doesDirectoryExist (path </> p)
  getDirectoryContents path >>= filterM isDirectory . Prelude.filter (\x -> head x /= '.')

-- | Read the Hackage database from the given 'FilePath' and return a
-- 'Hackage' map that provides fast access to its contents.

readHackage :: FilePath -> IO Hackage
readHackage root = runParIO $ do
  pkglist <- liftIO $ getSubDirs root
  hackage' <- parMapM (readHackagePackage root) $!! pkglist
  let hackage = [ (pkg, Data.Map.mapWithKey (mkPkgDescr pkg) version2buf) | (pkg, version2buf) <- hackage' ]

      mkPkgDescr :: String -> Version -> (ByteString, Meta) -> GenericPackageDescription
      mkPkgDescr pkg v (buf,meta) = cabal { packageDescription = pd { customFieldsPD = cf } }
        where
          cabal = parsePackage pkg v buf
          pd = packageDescription cabal
          cf = customFieldsPD pd ++ ("X-Cabal-File-Hash", mkSHA256 buf)
                                 :  [ ("X-Package-" ++ k, v') | (k,v') <- toList (hashes meta) ]

      mkSHA256 :: ByteString -> String
      mkSHA256 = showDigest . sha256 . fromStrict

  return (fromList hackage)

readHackagePackage :: FilePath -> FilePath -> ParIO (FilePath, Map Version (ByteString,Meta))
readHackagePackage root pkg = do
  versions <- liftIO $ getSubDirs (root </> pkg)
  x <- parMapM (readHackagePackageVersion root pkg) $!! versions
  return (pkg, fromList x)

readHackagePackageVersion :: FilePath -> FilePath -> FilePath -> ParIO (Version, (ByteString,Meta))
readHackagePackageVersion root pkg version = do
  let v = pVersion version
  cabal <- liftIO $ BS8.readFile (root </> pkg </> version </> pkg <.> "cabal")
  meta <- liftIO (BS8.readFile (root </> pkg </> version </> pkg <.> "json"))
  return (v, (cabal,parseMeta pkg v meta))

pVersion :: String -> Version
pVersion str = fromMaybe (error $ "hackage-db: cannot parse version " ++ show str) (simpleParse str)

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
    decodeUTF8 = toString . fromRep

data Meta = Meta { hashes :: Map String String {-, locations :: [String], pkgsize :: Int-} }
  deriving (Show, Generic)

instance NFData Meta where rnf = genericRnf

instance FromJSON Meta where
    parseJSON (Object v) = Meta <$>
                           v .: "package-hashes" {- <*>
                           v .: "package-locations" <*>
                           v .: "package-size" -}
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero

parseMeta :: String -> Version -> ByteString -> Meta
parseMeta pkg version buf =
  case eitherDecodeStrict buf of
    Left msg -> error $ "hackage-db: cannot decode JSON meta data for " ++ display pkgid ++ ":" ++ msg
    Right x -> x
  where
    pkgid = PackageIdentifier (PackageName pkg) version
