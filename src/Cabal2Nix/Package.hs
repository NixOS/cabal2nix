module Cabal2Nix.Package where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Functor ( (<$>), (<$) )
import Data.List ( isSuffixOf, isPrefixOf )
import Data.Maybe ( listToMaybe )
import Distribution.NixOS.Derivation.Cabal
import Distribution.NixOS.Fetch
import Distribution.Text ( simpleParse )
import System.Directory ( doesDirectoryExist, doesFileExist, createDirectoryIfMissing, getHomeDirectory, getDirectoryContents )
import System.Exit ( exitFailure )
import System.FilePath ( (</>), (<.>) )
import System.IO ( hPutStrLn, stderr, hPutStr )

import qualified Distribution.Hackage.DB as DB
import qualified Distribution.Package as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Parse as Cabal

data Package = Package
  { source :: DerivationSource
  , cabal :: Cabal.GenericPackageDescription
  }

getPackage :: Source -> IO Package
getPackage source = do
  (derivSource, pkgDesc) <- fetchOrFromDB source
  flip Package pkgDesc <$> maybe (sourceFromHackage (sourceHash source) $ showPackageIdentifier pkgDesc) return derivSource


fetchOrFromDB :: Source -> IO (Maybe DerivationSource, Cabal.GenericPackageDescription)
fetchOrFromDB src
  | "cabal://" `isPrefixOf` sourceUrl src = fmap ((,) Nothing) . fromDB . drop (length "cabal://") $ sourceUrl src
  | otherwise                             = do
    r <- fetch cabalFromPath src
    case r of
      Nothing -> hPutStrLn stderr "*** failed to fetch source. Does the URL exist?" >> exitFailure
      Just (derivSource, (externalSource, pkgDesc)) ->
        return (derivSource <$ guard externalSource, pkgDesc)

fromDB :: String -> IO Cabal.GenericPackageDescription
fromDB pkg = do
  pkgDesc <- (lookupVersion <=< DB.lookup name) <$> DB.readHackage
  case pkgDesc of
    Just r -> return r
    Nothing -> hPutStrLn stderr "*** no such package in the cabal database (did you run cabal update?). " >> exitFailure
 where
  Just pkgId = simpleParse pkg
  Cabal.PackageName name = Cabal.pkgName pkgId
  version = Cabal.pkgVersion pkgId

  lookupVersion :: DB.Map DB.Version Cabal.GenericPackageDescription -> Maybe Cabal.GenericPackageDescription
  lookupVersion
    | null (versionBranch version) = fmap snd . listToMaybe . reverse . DB.toAscList
    | otherwise                    = DB.lookup version


readFileMay :: String -> IO (Maybe String)
readFileMay file = do
  e <- doesFileExist file
  if e
    then Just <$> readFile file
    else return Nothing

hashCachePath :: String -> IO String
hashCachePath pid = do
  home <- getHomeDirectory
  let cacheDir = home </> ".cache/cabal2nix"
  createDirectoryIfMissing True cacheDir
  return $ cacheDir </> pid <.> "sha256"

sourceFromHackage :: Maybe String -> String -> IO DerivationSource
sourceFromHackage optHash pkgId = do
  cacheFile <- hashCachePath pkgId
  hash <- maybe (readFileMay cacheFile) (return . Just) optHash
  res <- runMaybeT $ fst <$> fetchWith (False, "url") (Source ("mirror://hackage/" ++ pkgId ++ ".tar.gz") "" hash)
  case res of
    Just r -> writeFile cacheFile (derivHash r) >> return r
    Nothing -> do
      hPutStr stderr $ unlines
        [ "*** cannot compute hash. (Not a hackage project?)"
        , " Specify hash explicitly via --sha256 and add appropriate \"src\" attribute"
        , " to resulting nix expression."
        ]
      exitFailure

showPackageIdentifier :: Cabal.GenericPackageDescription -> String
showPackageIdentifier pkgDesc = name ++ "-" ++ showVersion version where
  pkgId = Cabal.package . Cabal.packageDescription $ pkgDesc
  Cabal.PackageName name = Cabal.packageName pkgId
  version = Cabal.packageVersion pkgId

cabalFromPath :: FilePath -> MaybeT IO (Bool, Cabal.GenericPackageDescription)
cabalFromPath path = do
  d <- liftIO $ doesDirectoryExist path
  (,) d <$> if d
    then cabalFromDirectory path
    else cabalFromFile      path

cabalFromDirectory :: FilePath -> MaybeT IO Cabal.GenericPackageDescription
cabalFromDirectory dir = do
  cabals <- liftIO $ getDirectoryContents dir >>= filterM doesFileExist . map (dir </>) . filter (".cabal" `isSuffixOf`)
  case cabals of
    [cabalFile] -> cabalFromFile cabalFile
    _       -> liftIO $ hPutStrLn stderr "*** found zero or more than one cabal file. Exiting." >> exitFailure

cabalFromFile :: FilePath -> MaybeT IO Cabal.GenericPackageDescription
cabalFromFile file = do
  content <- liftIO $ readFile file
  case Cabal.parsePackageDescription content of
    Cabal.ParseFailed _ -> mzero
    Cabal.ParseOk     _ a -> return a
