module Distribution.Nixpkgs.Haskell.PackageSourceSpec
  ( Package(..), getPackage, sourceFromHackage
  ) where

import qualified Control.Exception as Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.List ( isSuffixOf, isPrefixOf )
import qualified Data.Map as DB
import Data.Maybe
import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.Hashes
import qualified Distribution.Nixpkgs.Haskell.Hackage as DB
import qualified Distribution.Package as Cabal
import Distribution.PackageDescription
import qualified Distribution.PackageDescription as Cabal
import Distribution.PackageDescription.Parse as Cabal
import Distribution.Text ( simpleParse, display )
import Distribution.Version
import OpenSSL.Digest ( digestString, digestByName )
import System.Directory ( doesDirectoryExist, doesFileExist, createDirectoryIfMissing, getHomeDirectory, getDirectoryContents )
import System.Exit ( exitFailure )
import System.FilePath ( (</>), (<.>) )
import System.IO ( hPutStrLn, stderr, hPutStr )

data Package = Package
  { pkgSource :: DerivationSource
  , pkgCabal :: Cabal.GenericPackageDescription
  }
  deriving (Show)

getPackage :: Maybe FilePath -> Source -> IO Package
getPackage optHackageDB source = do
  (derivSource, pkgDesc) <- fetchOrFromDB optHackageDB source
  flip Package pkgDesc <$> maybe (sourceFromHackage (sourceHash source) (showPackageIdentifier pkgDesc) $ sourceCabalDir source) return derivSource

fetchOrFromDB :: Maybe FilePath -> Source -> IO (Maybe DerivationSource, Cabal.GenericPackageDescription)
fetchOrFromDB optHackageDB src
  | "cabal://" `isPrefixOf` sourceUrl src = fromDB optHackageDB . drop (length "cabal://") $ sourceUrl src
  | otherwise                             = do
    r <- fetch (\dir -> cabalFromPath (dir </> sourceCabalDir src)) src
    case r of
      Nothing -> fail "Failed to fetch source. Does the URL exist?"
      Just (derivSource, (externalSource, pkgDesc)) -> do
        return (derivSource <$ guard externalSource, pkgDesc)

fromDB :: Maybe FilePath -> String -> IO (Maybe DerivationSource, Cabal.GenericPackageDescription)
fromDB optHackageDB pkg = do
  dbPath <- maybe DB.hackageTarball return optHackageDB
  db <- DB.readTarball Nothing dbPath
  vd <- maybe unknownPackageError return (DB.lookup name db >>= lookupVersion)
  let ds = case DB.tarballSha256 vd of
             Nothing -> Nothing
             Just hash -> Just (DerivationSource "url" url "" hash)
  return (ds, setCabalFileHash (DB.cabalFileSha256 vd) (DB.cabalFile vd))
 where
  pkgId :: Cabal.PackageIdentifier
  pkgId = fromMaybe (error ("invalid Haskell package id " ++ show pkg)) (simpleParse pkg)
  name = Cabal.packageName pkgId

  unknownPackageError = fail "No such package in the cabal database. Did you run cabal update?"

  url = "mirror://hackage/" ++ display pkgId ++ ".tar.gz"

  version :: Version
  version = Cabal.packageVersion pkgId

  lookupVersion :: DB.Map Version DB.VersionData -> Maybe DB.VersionData
  -- No version is specified, pick latest one
  lookupVersion m | version == nullVersion  = fmap snd (listToMaybe (DB.toDescList m))
  lookupVersion m                           = DB.lookup version m

readFileMay :: FilePath -> IO (Maybe String)
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

sourceFromHackage :: Hash -> String -> FilePath -> IO DerivationSource
sourceFromHackage optHash pkgId cabalDir = do
  cacheFile <- hashCachePath pkgId
  cachedHash <-
    case optHash of
      Certain h -> return . Certain $ h
      Guess   h -> return . Guess $ h
      _         -> fmap (maybe UnknownHash Certain) . readFileMay $ cacheFile
  let url = "mirror://hackage/" ++ pkgId ++ ".tar.gz"

  -- Use the cached hash (either from cache file or given on cmdline via sha256 opt)
  -- if available, otherwise download from hackage to compute hash.
  case cachedHash of
    Guess hash -> return $ DerivationSource "url" url "" hash
    Certain hash ->
      -- We need to force the hash here. If we didn't do this, then when reading the
      -- hash from the cache file, the cache file will still be open for reading
      -- (because lazy io) when writeFile opens the file again for writing. By forcing
      -- the hash here, we ensure that the file is closed before opening it again.
      seq (length hash) $
      DerivationSource "url" url "" hash <$ writeFile cacheFile hash
    UnknownHash -> do
      maybeHash <- runMaybeT (derivHash . fst <$> fetchWith (False, "url", []) (Source url "" UnknownHash cabalDir))
      case maybeHash of
        Just hash ->
          seq (length hash) $
          DerivationSource "url" url "" hash <$ writeFile cacheFile hash
        Nothing -> do
          hPutStr stderr $ unlines
            [ "*** cannot compute hash. (Not a hackage project?)"
            , " If your project is not on hackage, please supply the path to the root directory of"
            , " the project, not to the cabal file."
            , ""
            , " If your project is on hackage but you still want to specify the hash manually, you"
            , " can use the --sha256 option."
            ]
          exitFailure

showPackageIdentifier :: Cabal.GenericPackageDescription -> String
showPackageIdentifier pkgDesc = name ++ "-" ++ display version where
  pkgId = Cabal.package . Cabal.packageDescription $ pkgDesc
  name = Cabal.unPackageName (Cabal.packageName pkgId)
  version = Cabal.packageVersion pkgId

cabalFromPath :: FilePath -> MaybeT IO (Bool, Cabal.GenericPackageDescription)
cabalFromPath path = do
  d <- liftIO $ doesDirectoryExist path
  (,) d <$> if d
    then cabalFromDirectory  path
    else cabalFromFile False path

cabalFromDirectory :: FilePath -> MaybeT IO Cabal.GenericPackageDescription
cabalFromDirectory dir = do
  cabals <- liftIO $ getDirectoryContents dir >>= filterM doesFileExist . map (dir </>) . filter (".cabal" `isSuffixOf`)
  case cabals of
    [cabalFile] -> cabalFromFile True cabalFile
    _       -> liftIO $ fail ("Found zero or more than one cabal files: " ++ show cabals ++ ". Exiting.")

handleIO :: (Exception.IOException -> IO a) -> IO a -> IO a
handleIO = Exception.handle

cabalFromFile :: Bool -> FilePath -> MaybeT IO Cabal.GenericPackageDescription
cabalFromFile failHard file =
  -- readFile throws an error if it's used on binary files which contain sequences
  -- that do not represent valid characters. To catch that exception, we need to
  -- wrap the whole block in `catchIO`, because of lazy IO. The `case` will force
  -- the reading of the file, so we will always catch the expression here.
  MaybeT $ handleIO (\err -> Nothing <$ hPutStrLn stderr ("*** parsing cabal file: " ++ show err)) $ do
    buf <- readFile file
    let hash = printSHA256 (digestString (digestByName "sha256") buf)
    case parseGenericPackageDescription buf of
      ParseFailed perr -> if failHard
                             then fail ("cannot parse " ++ show file ++ ": " ++ show perr)
                             else return Nothing
      ParseOk _ pkg    -> return $ Just $ setCabalFileHash hash pkg

setCabalFileHash :: String -> GenericPackageDescription -> GenericPackageDescription
setCabalFileHash sha256 gpd = gpd { packageDescription = (packageDescription gpd) {
                                      customFieldsPD = ("X-Cabal-File-Hash", sha256) : customFieldsPD (packageDescription gpd)
                                    }
                                  }
