{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module HackageGit where

import Control.Monad
import Control.Lens hiding ( (<.>) )
import Data.Aeson
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 ( fromStrict )
import Data.Digest.Pure.SHA ( sha256, showDigest )
import Data.Map as Map
import Data.Set as Set
import Data.String
import Data.String.UTF8 ( toString, fromRep )
import Distribution.Nixpkgs.Haskell.OrphanInstances ( )
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse ( parsePackageDescription, ParseResult(..) )
import Distribution.Text
import Distribution.Version
import System.Directory
import System.FilePath

type Hackage = Map PackageName (Set Version)

readHackage :: FilePath -> IO Hackage
readHackage path = getSubDirs path >>= foldM discoverPackageVersions mempty
  where
    discoverPackageVersions :: Hackage -> String -> IO Hackage
    discoverPackageVersions db pkg = do
      vs <- getSubDirs (path </> pkg)
      return (Map.insert (PackageName pkg) (Set.fromList (Prelude.map fromString vs)) db)

getSubDirs :: FilePath -> IO [FilePath]
getSubDirs path = do
  let isDirectory p = doesDirectoryExist (path </> p)
  getDirectoryContents path >>= filterM isDirectory . Prelude.filter (\x -> head x /= '.')

decodeUTF8 :: ByteString -> String
decodeUTF8 = toString . fromRep

type SHA256Hash = String

readPackage :: FilePath -> PackageIdentifier -> IO (GenericPackageDescription, SHA256Hash)
readPackage dirPrefix (PackageIdentifier name version) = do
  let cabalFile = dirPrefix </> unPackageName name </> display version </> unPackageName name <.> "cabal"
  buf <- BS.readFile cabalFile
  cabal <- case parsePackageDescription (decodeUTF8 buf) of
             ParseOk _ a  -> return a
             ParseFailed err -> fail (cabalFile ++ ": " ++ show err)
  return (cabal, mkSHA256 buf)

mkSHA256 :: ByteString -> SHA256Hash
mkSHA256 = showDigest . sha256 . fromStrict

declareLenses [d|
  data Meta = Meta { hashes :: Map String String
                   , locations :: [String]
                   , pkgsize :: Int
                   }
    deriving (Show)
  |]

instance FromJSON Meta where
  parseJSON (Object v) = Meta
                         <$> v .: "package-hashes"
                         <*> v .: "package-locations"
                         <*> v .: "package-size"
  parseJSON o          = fail ("invalid Cabal metadata: " ++ show o)

readPackageMeta :: FilePath -> PackageIdentifier -> IO Meta
readPackageMeta dirPrefix (PackageIdentifier name version) = do
  let metaFile = dirPrefix </> unPackageName name </> display version </> unPackageName name <.> "json"
  buf <- BS.readFile metaFile
  case eitherDecodeStrict buf of
    Left msg -> fail (metaFile ++ ": " ++ msg)
    Right x  -> return x
