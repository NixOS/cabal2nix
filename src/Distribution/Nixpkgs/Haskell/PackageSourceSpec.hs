{-# LANGUAGE CPP #-}
module Distribution.Nixpkgs.Haskell.PackageSourceSpec
  ( Package(..), getPackage, getPackage', loadHackageDB, sourceFromHackage
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8 as BS
import Data.List ( isSuffixOf, isPrefixOf )
import qualified Data.Map as DB
import Data.Maybe
import qualified Data.String.UTF8 as UTF8
import Data.Time
import Distribution.Nixpkgs.CabalCompat
import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.Hashes
import qualified Distribution.Nixpkgs.Haskell.Hackage as DB
import qualified Distribution.Package as Cabal
import Distribution.PackageDescription
import qualified Distribution.PackageDescription as Cabal
import Distribution.Text ( simpleParse, display )
import Distribution.Version
#if MIN_VERSION_hpack(0,28,0)
import qualified Hpack.Render as Hpack
#else
import qualified Hpack.Run as Hpack
#endif
import qualified Hpack.Config as Hpack
import OpenSSL.Digest ( digestString, digestByName )
import System.Directory ( doesDirectoryExist, doesFileExist, createDirectoryIfMissing, getHomeDirectory, getDirectoryContents )
import System.Exit ( exitFailure )
import System.FilePath ( (</>), (<.>) )
import System.IO ( hPutStrLn, stderr, hPutStr )

data Package = Package
  { pkgSource   :: DerivationSource
  , pkgRanHpack :: Bool -- ^ If hpack generated a new cabal file
  , pkgCabal    :: Cabal.GenericPackageDescription
  }
  deriving (Show)

getPackage :: Bool
           -- ^ Whether hpack should regenerate the cabal file.
           -> Maybe FilePath
           -- ^ The path to the Hackage database.
           -> Maybe UTCTime
           -- ^ If we have hackage-snapshot time.
           -> Source
           -> IO Package
getPackage optHpack optHackageDB optHackageSnapshot source = do
  getPackage' optHpack (loadHackageDB optHackageDB optHackageSnapshot) source

getPackage' :: Bool
            -- ^ Whether hpack should regenerate the cabal file.
            -> IO DB.HackageDB
            -> Source
            -> IO Package
getPackage' optHpack hackageDB source = do
  (derivSource, ranHpack, pkgDesc) <- fetchOrFromDB optHpack hackageDB source
  (\s -> Package s ranHpack pkgDesc) <$> maybe (sourceFromHackage (sourceHash source) (showPackageIdentifier pkgDesc) $ sourceCabalDir source) return derivSource

fetchOrFromDB :: Bool
              -- ^ Whether hpack should regenerate the cabal file
              -> IO DB.HackageDB
              -> Source
              -> IO (Maybe DerivationSource, Bool, Cabal.GenericPackageDescription)
fetchOrFromDB optHpack hackageDB src
  | "cabal://" `isPrefixOf` sourceUrl src = do
      (msrc, pkgDesc) <- fromDB hackageDB . drop (length "cabal://") $ sourceUrl src
      return (msrc, False, pkgDesc)
  | otherwise                             = do
    r <- fetch (\dir -> cabalFromPath optHpack (dir </> sourceCabalDir src)) src
    case r of
      Nothing -> fail "Failed to fetch source. Does the URL exist?"
      Just (derivSource, (externalSource, ranHpack, pkgDesc)) -> do
        return (derivSource <$ guard externalSource, ranHpack, pkgDesc)

loadHackageDB :: Maybe FilePath
              -- ^ The path to the Hackage database.
              -> Maybe UTCTime
              -- ^ If we have hackage-snapshot time.
              -> IO DB.HackageDB
loadHackageDB optHackageDB optHackageSnapshot = do
  dbPath <- maybe DB.hackageTarball return optHackageDB
  DB.readTarball optHackageSnapshot dbPath

fromDB :: IO DB.HackageDB
       -> String
       -> IO (Maybe DerivationSource, Cabal.GenericPackageDescription)
fromDB hackageDBIO pkg = do
  hackageDB <- hackageDBIO
  vd <- maybe unknownPackageError return (DB.lookup name hackageDB >>= lookupVersion)
  let ds = case DB.tarballSha256 vd of
             Nothing -> Nothing
             Just hash -> Just (DerivationSource "url" url "" hash)
  return (ds, setCabalFileHash (DB.cabalFileSha256 vd) (DB.cabalFile vd))
 where
  pkgId :: Cabal.PackageIdentifier
  pkgId = fromMaybe (error ("invalid Haskell package id " ++ show pkg)) (simpleParse pkg)
  name = Cabal.packageName pkgId

  unknownPackageError = fail $ "No such package " ++ display pkgId ++ " in the cabal database. Did you run cabal update?"

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

cabalFromPath :: Bool -- ^ Whether hpack should regenerate the cabal file
              -> FilePath -> MaybeT IO (Bool, Bool, Cabal.GenericPackageDescription)
cabalFromPath optHpack path = do
  d <- liftIO $ doesDirectoryExist path
  if d
  then do
    (ranHpack, pkg) <- cabalFromDirectory optHpack path
    return (d, ranHpack, pkg)
  else (,,) d False <$> cabalFromFile False path

cabalFromDirectory :: Bool -- ^ Whether hpack should regenerate the cabal file
                   -> FilePath -> MaybeT IO (Bool, Cabal.GenericPackageDescription)
cabalFromDirectory True dir = hpackDirectory dir
cabalFromDirectory False dir = do
  cabals <- liftIO $ getDirectoryContents dir >>= filterM doesFileExist . map (dir </>) . filter (".cabal" `isSuffixOf`)
  case cabals of
    [] -> do
      liftIO $ hPutStrLn stderr "*** found zero cabal files. Trying hpack..."
      hpackDirectory dir
    [cabalFile] -> (,) False <$> cabalFromFile True cabalFile
    _       -> liftIO $ fail ("*** found more than one cabal file (" ++ show cabals ++ "). Exiting.")

hpackDirectory :: FilePath -> MaybeT IO (Bool, Cabal.GenericPackageDescription)
hpackDirectory dir = do
#if MIN_VERSION_hpack(0,28,0)
  mPackage <- fmap (fmap Hpack.decodeResultPackage) $ liftIO $ Hpack.readPackageConfig
    $ Hpack.defaultDecodeOptions { Hpack.decodeOptionsTarget = dir </> "package.yaml" }
#else
  mPackage <- fmap (fmap snd) $ liftIO $ Hpack.readPackageConfig $ dir </> "package.yaml"
#endif
  case mPackage of
    Left err -> liftIO $ hPutStrLn stderr ("*** hpack error: " ++ show err ++ ". Exiting.") >> exitFailure
    Right pkg' -> do
#if MIN_VERSION_hpack(0,28,0)
      let hpackOutput = Hpack.renderPackage [] pkg'
#else
      let hpackOutput = Hpack.renderPackage Hpack.defaultRenderSettings 2 [] [] pkg'
#endif
      let hash = printSHA256 $ digestString (digestByName "sha256") hpackOutput
      case parseGenericPackageDescription $ UTF8.toRep $ UTF8.fromString hpackOutput of
        Left perr -> liftIO $ do
          hPutStrLn stderr $ "*** cannot parse hpack output: " ++ show perr
          hPutStrLn stderr $ "*** hpack output:\n" ++ hpackOutput
          fail "*** Exiting."
        Right pkg -> MaybeT $ return $ Just $ (,) True $ setCabalFileHash hash pkg

cabalFromFile :: Bool -> FilePath -> MaybeT IO Cabal.GenericPackageDescription
cabalFromFile failHard file =
  MaybeT $ do
    buf <- BS.readFile file
    let hash = printSHA256 (digestString (digestByName "sha256") $ BS.unpack buf)
    case parseGenericPackageDescription buf of
      Left perr -> if failHard
                             then fail ("cannot parse " ++ show file ++ ": " ++ show perr)
                             else return Nothing
      Right pkg    -> return $ Just $ setCabalFileHash hash pkg

setCabalFileHash :: String -> GenericPackageDescription -> GenericPackageDescription
setCabalFileHash sha256 gpd = gpd { packageDescription = (packageDescription gpd) {
                                      customFieldsPD = ("X-Cabal-File-Hash", sha256) : customFieldsPD (packageDescription gpd)
                                    }
                                  }
