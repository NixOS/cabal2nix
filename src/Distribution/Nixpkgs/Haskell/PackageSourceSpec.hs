module Distribution.Nixpkgs.Haskell.PackageSourceSpec
  ( Package(..), getPackage, getPackage', loadHackageDB, sourceFromHackage
  ) where

import qualified Control.Exception as Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Bifunctor
import qualified Data.ByteString.Char8 as BS
import Data.List ( isSuffixOf, isPrefixOf )
import qualified Data.Map as DB
import Data.Maybe
import Data.Time
import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.Hashes
import qualified Distribution.Nixpkgs.Haskell.Hackage as DB
import qualified Distribution.Package as Cabal
import Distribution.PackageDescription
import qualified Distribution.PackageDescription as Cabal
import Distribution.PackageDescription.Parsec as Cabal
import Distribution.Parsec.Common (showPError)
import Distribution.Text ( simpleParse, display )
import Distribution.Version
import qualified Hpack.Config as Hpack
import qualified Hpack.Render as Hpack
import OpenSSL.Digest ( digest, digestByName )
import System.Directory ( doesDirectoryExist, doesFileExist, createDirectoryIfMissing, getHomeDirectory, getDirectoryContents )
import System.Exit ( exitFailure )
import System.FilePath ( (</>), (<.>) )
import System.IO
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

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
getPackage optHpack optHackageDB optHackageSnapshot =
  getPackage' optHpack (loadHackageDB optHackageDB optHackageSnapshot)

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
      Just (derivSource, (externalSource, ranHpack, pkgDesc)) ->
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
  useHpack <- liftIO $ doesFileExist (dir </> "package.yaml")
  if useHpack
    then do
      liftIO $ hPutStrLn stderr "*** found package.yaml. Using hpack..."
      hpackDirectory dir
    else do
      cabals <- liftIO $ getDirectoryContents dir >>= filterM doesFileExist . map (dir </>) . filter (".cabal" `isSuffixOf`)
      case cabals of
        [] -> do
          fail "*** Found neither a .cabal file nor package.yaml. Exiting."
        [cabalFile] -> (,) False <$> cabalFromFile True cabalFile
        _ -> liftIO $ fail ("*** found more than one cabal file (" ++ show cabals ++ "). Exiting.")

handleIO :: (Exception.IOException -> IO a) -> IO a -> IO a
handleIO = Exception.handle

encodeUtf8 :: String -> BS.ByteString
encodeUtf8 = T.encodeUtf8 . T.pack

hpackDirectory :: FilePath -> MaybeT IO (Bool, Cabal.GenericPackageDescription)
hpackDirectory dir = do
  mPackage <- liftIO $ Hpack.readPackageConfig Hpack.defaultDecodeOptions {Hpack.decodeOptionsTarget = dir </> Hpack.packageConfig}
  case mPackage of
    Left err -> liftIO $ hPutStrLn stderr ("*** hpack error: " ++ show err ++ ". Exiting.") >> exitFailure
    Right (Hpack.DecodeResult pkg' _ _) -> do
      let hpackOutput = encodeUtf8 $ Hpack.renderPackage [] pkg'
          hash = printSHA256 $ digest (digestByName "sha256") hpackOutput
      case runParseGenericPackageDescription "<hpack output>" hpackOutput of
        Left msg -> liftIO $ do
          hPutStrLn stderr "*** hpack output:"
          BS.hPutStrLn stderr hpackOutput
          hPutStrLn stderr "*** cannot parse hpack output:"
          hPutStrLn stderr msg
          fail "*** Exiting."
        Right pkg -> MaybeT $ return $ Just $ (,) True $ setCabalFileHash hash pkg

cabalFromFile :: Bool -> FilePath -> MaybeT IO Cabal.GenericPackageDescription
cabalFromFile failHard file =
  -- hGetContents throws an error if it's used on files which contain sequences
  -- that do not represent valid characters. To catch that exception, we need to
  -- wrap the whole block in `catchIO`.
  MaybeT $ handleIO (\err -> Nothing <$ hPutStrLn stderr ("*** parsing cabal file: " ++ show err)) $ do
    buf <- BS.readFile file
    let hash = printSHA256 (digest (digestByName "sha256") buf)
    case runParseGenericPackageDescription file buf of
      Left msg | failHard -> liftIO $ do
          hPutStrLn stderr $ "*** cannot parse " ++ show file ++ ":"
          hPutStrLn stderr msg
          fail "*** Exiting."
      Left _ -> return Nothing
      Right pkg -> return $ Just $ setCabalFileHash hash pkg

runParseGenericPackageDescription
  :: FilePath
  -> BS.ByteString
  -> Either String Cabal.GenericPackageDescription
runParseGenericPackageDescription fpath
  = first (unlines . fmap (showPError fpath) . snd)
  . snd . runParseResult
  . parseGenericPackageDescription

setCabalFileHash :: String -> GenericPackageDescription -> GenericPackageDescription
setCabalFileHash sha256 gpd = gpd { packageDescription = (packageDescription gpd) {
                                      customFieldsPD = ("X-Cabal-File-Hash", sha256) : customFieldsPD (packageDescription gpd)
                                    }
                                  }
