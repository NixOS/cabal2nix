{-# LANGUAGE OverloadedStrings #-}

module Cabal2Nix.HackageGit ( Hackage, readHackage, module Data.Map )  where

import Control.Monad
import Data.Aeson
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 ( fromStrict )
import Data.Digest.Pure.SHA ( sha256, showDigest )
import Data.List ( foldl' )
import Data.Map
import Data.Maybe ( fromMaybe, mapMaybe )
import Data.String.UTF8 ( toString, fromRep )
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse ( parsePackageDescription, ParseResult(..) )
import Distribution.Text ( simpleParse )
import Distribution.Version ( Version )
import System.Directory
import System.FilePath

type Hackage = Map String (Map Version (IO GenericPackageDescription))

readHackage :: FilePath -> IO ([Dependency], Hackage)
readHackage path = do
    db <- getSubDirs path >>= foldM discoverPackageVersions empty
    preferred <- parsePreferredVersions <$> BS.readFile (path </> "preferred-versions")
    return $ (preferred, mapWithKey (\pkg -> Data.List.foldl' (makeVersionMap pkg) empty) db)
  where
    discoverPackageVersions :: Map String [String] -> String -> IO (Map String [String])
    discoverPackageVersions db pkg = getSubDirs (path </> pkg) >>= \vs -> return (insert pkg vs db)

    makeVersionMap :: String -> Map Version (IO GenericPackageDescription) -> String -> Map Version (IO GenericPackageDescription)
    makeVersionMap pkg db v = insert (parseVersion v) (readHackagePackage path pkg v) db

readHackagePackage :: FilePath -> String -> String -> IO GenericPackageDescription
readHackagePackage path pkg version = do
  let cabalFile = path </> pkg </> version </> pkg <.> "cabal"
      metaFile = path </> pkg </> version </> pkg <.> "json"
  parseHackagePackage (path </> pkg </> version) <$> BS.readFile cabalFile <*> BS.readFile metaFile

getSubDirs :: FilePath -> IO [FilePath]
getSubDirs path = do
  let isDirectory p = doesDirectoryExist (path </> p)
  getDirectoryContents path >>= filterM isDirectory . Prelude.filter (\x -> head x /= '.')

parsePreferredVersions :: ByteString -> [Dependency]
parsePreferredVersions = Data.Maybe.mapMaybe parsePreferredVersionsLine . lines . decodeUTF8

parsePreferredVersionsLine :: String -> Maybe Dependency
parsePreferredVersionsLine ('-':'-':_) = Nothing
parsePreferredVersionsLine l = case simpleParse l of
                                 Just c -> Just c
                                 Nothing -> error ("invalid preferred-versions line: " ++ show l)

addCustomFields :: [(String, String)] -> GenericPackageDescription -> GenericPackageDescription
addCustomFields fields cabal = cabal { packageDescription = pd { customFieldsPD = cf } }
  where cf = fields ++ customFieldsPD pd
        pd = packageDescription cabal

parseHackagePackage :: FilePath -> ByteString -> ByteString -> GenericPackageDescription
parseHackagePackage path cabalBuf metaBuf = case parsePackageDescription (decodeUTF8 cabalBuf) of
    ParseOk _ a -> addCustomFields metaFields a
    ParseFailed err -> error $ path ++ ": " ++ show err
  where
    metaFields = ("X-Cabal-File-Hash", mkSHA256 cabalBuf) :
               [ ("X-Package-" ++ k, v') | (k,v') <- toList (hashes meta) ]
    meta = parseMeta path metaBuf

parseVersion :: String -> Version
parseVersion str = fromMaybe (error $ "hackage-db: cannot parse version " ++ show str) (simpleParse str)

decodeUTF8 :: ByteString -> String
decodeUTF8 = toString . fromRep

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

parseMeta :: FilePath -> ByteString -> Meta
parseMeta path buf =
  case eitherDecodeStrict buf of
    Left msg -> error $ "hackage-db: cannot decode JSON meta data for " ++ path ++ ":" ++ msg
    Right x -> x
