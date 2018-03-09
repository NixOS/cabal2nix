{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module HackageGit
  ( Hackage, readHackage, readPackage, readPackageMeta
  , SHA256Hash, hashes
  )
  where

import Control.Lens hiding ( (<.>) )
import Control.Monad
import Data.Aeson
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import Data.Map as Map
import Data.Set as Set
import Data.String
import Data.String.UTF8 ( toString, fromRep )
import Distribution.Nixpkgs.Hashes
import Distribution.Nixpkgs.Haskell.OrphanInstances ( )
import Distribution.Package
import Distribution.PackageDescription
#if MIN_VERSION_Cabal(2,2,0)
import Distribution.PackageDescription.Parsec
#else
import Distribution.PackageDescription.Parse
#endif
                                             ( parseGenericPackageDescription, ParseResult(..), runParseResult )
import Distribution.Text
import Distribution.Version
import OpenSSL.Digest ( digest, digestByName )
import System.Directory
import System.FilePath

type Hackage = Map PackageName (Set Version)

readHackage :: FilePath -> IO Hackage
readHackage path = getSubDirs path >>= foldM discoverPackageVersions mempty
  where
    discoverPackageVersions :: Hackage -> String -> IO Hackage
    discoverPackageVersions db pkg = do
      vs <- getSubDirs (path </> pkg)
      return (Map.insert (mkPackageName pkg) (Set.fromList (Prelude.map fromString vs)) db)

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
  cabal <-
#if MIN_VERSION_Cabal(2,2,0)
           case snd $ runParseResult $ parseGenericPackageDescription buf of
             Right a -> return a
             Left err ->
#else
           case parseGenericPackageDescription (decodeUTF8 buf) of
             ParseOk _ a  -> return a
             ParseFailed err ->
#endif
               fail (cabalFile ++ ": " ++ show err)
  return (cabal, printSHA256 (digest (digestByName "sha256") buf))

declareLenses [d|
  data Meta = Meta { hashes :: Map String String
                   }
    deriving (Show)
  |]

instance FromJSON Meta where
  parseJSON (Object v) = Meta
                         <$> v .: "package-hashes"
  parseJSON o          = fail ("invalid Cabal metadata: " ++ show o)

readPackageMeta :: FilePath -> PackageIdentifier -> IO Meta
readPackageMeta dirPrefix (PackageIdentifier name version) = do
  let metaFile = dirPrefix </> unPackageName name </> display version </> unPackageName name <.> "json"
  buf <- BS.readFile metaFile
  case eitherDecodeStrict buf of
    Left msg -> fail (metaFile ++ ": " ++ msg)
    Right x  -> return $ over (hashes . ix "SHA256") (printSHA256 . packHex) x
