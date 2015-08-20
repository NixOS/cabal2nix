{-# LANGUAGE OverloadedStrings #-}

module Distribution.Nixpkgs.Haskell.FromCabal.HackageGit ( Hackage, readHackage, module Data.Map )  where

import Control.Monad
import Data.Aeson
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 ( fromStrict )
import Data.Digest.Pure.SHA ( sha256, showDigest )
import Data.List ( foldl' )
import Data.Map
import Data.Maybe ( fromMaybe )
import Data.String.UTF8 ( toString, fromRep )
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse ( parsePackageDescription, ParseResult(..) )
import Distribution.Text ( simpleParse )
import Distribution.Version ( Version )
import System.Directory
import System.FilePath
import System.IO.Unsafe

type Hackage = Map String (Map Version GenericPackageDescription)

readHackage :: FilePath -> IO Hackage
readHackage path = do db <- getSubDirs path >>= foldM discoverPackageVersions empty
                      return $ mapWithKey (\pkg -> Data.List.foldl' (makeVersionMap pkg) empty) db
  where
    discoverPackageVersions :: Map String [String] -> String -> IO (Map String [String])
    discoverPackageVersions db pkg = getSubDirs (path </> pkg) >>= \vs -> return (insert pkg vs db)

    makeVersionMap :: String -> Map Version GenericPackageDescription -> String -> Map Version GenericPackageDescription
    makeVersionMap pkg db v = insert (parseVersion v) (unsafePerformIO (readHackagePackage path pkg v)) db

readHackagePackage :: FilePath -> String -> String -> IO GenericPackageDescription
readHackagePackage path pkg version = do
  let cabalFile = path </> pkg </> version </> pkg <.> "cabal"
      metaFile = path </> pkg </> version </> pkg <.> "json"
  buf <- BS.readFile cabalFile
  meta <- parseMeta path version <$> BS.readFile metaFile
  cabal <- case parsePackageDescription (decodeUTF8 buf) of
    ParseOk _ a  -> return a
    ParseFailed err -> fail $ cabalFile ++ ": " ++ show err
  let cabal' = cabal { packageDescription = pd { customFieldsPD = cf } }
      pd = packageDescription cabal
      cf = customFieldsPD pd ++ ("X-Cabal-File-Hash", mkSHA256 buf)
                             :  [ ("X-Package-" ++ k, v') | (k,v') <- toList (hashes meta) ]
  return cabal'

parseVersion :: String -> Version
parseVersion str = fromMaybe (error $ "hackage-db: cannot parse version " ++ show str) (simpleParse str)

decodeUTF8 :: ByteString -> String
decodeUTF8 = toString . fromRep

getSubDirs :: FilePath -> IO [FilePath]
getSubDirs path = do
  let isDirectory p = doesDirectoryExist (path </> p)
  getDirectoryContents path >>= filterM isDirectory . Prelude.filter (\x -> head x /= '.')

mkSHA256 :: ByteString -> String
mkSHA256 = showDigest . sha256 . fromStrict

data Meta = Meta { hashes :: Map String String {-, locations :: [String], pkgsize :: Int-} }
  deriving (Show)

instance FromJSON Meta where
    parseJSON (Object v) = Meta <$>
                           v .: "package-hashes" {- <*>
                           v .: "package-locations" <*>
                           v .: "package-size" -}
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero

parseMeta :: String -> String -> ByteString -> Meta
parseMeta pkg version buf =
  case eitherDecodeStrict buf of
    Left msg -> error $ "hackage-db: cannot decode JSON meta data for " ++ pkg ++ "-" ++ version ++ ":" ++ msg
    Right x -> x
