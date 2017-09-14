module Distribution.Nixpkgs.Haskell.PackageSourceSpec
  ( Package(..), getPackage, sourceFromHackage
  ) where

import qualified Control.Exception as Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.List ( isSuffixOf, isPrefixOf )
import Data.Maybe
import Distribution.Hackage.DB.Parsed
import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.Hashes
import qualified Distribution.Nixpkgs.Haskell.Hackage as DB
import qualified Distribution.Package as Cabal
import qualified Distribution.Version as Cabal
import Distribution.PackageDescription
import qualified Distribution.PackageDescription as Cabal
import Distribution.Text ( simpleParse, display )
import OpenSSL.Digest ( digest, digestByName )
import qualified Hpack.Run as Hpack
import qualified Hpack.Config as Hpack
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

getPackage :: Bool -- ^ Whether hpack should regenerate the cabal file
           -> Maybe String -> Source -> IO Package
getPackage optHpack optHackageDB source = do
  (derivSource, ranHpack, pkgDesc) <- fetchOrFromDB optHpack optHackageDB source
  (\s -> Package s ranHpack pkgDesc) <$> maybe (sourceFromHackage (sourceHash source) (showPackageIdentifier pkgDesc) $ sourceCabalDir source) return derivSource

fetchOrFromDB :: Bool -- ^ Whether hpack should regenerate the cabal file
              -> Maybe String -> Source -> IO (Maybe DerivationSource, Bool, Cabal.GenericPackageDescription)
fetchOrFromDB optHpack optHackageDB src
  | "cabal://" `isPrefixOf` sourceUrl src = fmap ((,,) Nothing False) . fromDB optHackageDB . drop (length "cabal://") $ sourceUrl src
  | otherwise                             = do
    r <- fetch (\dir -> cabalFromPath optHpack (dir </> sourceCabalDir src)) src
    case r of
      Nothing ->
        hPutStrLn stderr "*** failed to fetch source. Does the URL exist?" >> exitFailure
      Just (derivSource, (externalSource, ranHpack, pkgDesc)) -> do
        return (derivSource <$ guard externalSource, ranHpack, pkgDesc)

fromDB :: Maybe String -> String -> IO Cabal.GenericPackageDescription
fromDB optHackageDB pkg = do
  pkgDesc <- (lookupVersion <=< DB.lookup name) <$> maybe DB.readHashedHackage DB.readHashedHackage' optHackageDB
  case pkgDesc of
    Just r -> return r
    Nothing -> hPutStrLn stderr "*** no such package in the cabal database (did you run cabal update?). " >> exitFailure
 where
  pkgId :: Cabal.PackageIdentifier
  pkgId = fromMaybe (error ("invalid Haskell package id " ++ show pkg)) (simpleParse pkg)
  name = Cabal.unPackageName (Cabal.packageName pkgId)

  version :: [Int]
  version = Cabal.versionNumbers $ Cabal.packageVersion pkgId

  lookupVersion :: DB.Map DB.Version Cabal.GenericPackageDescription -> Maybe Cabal.GenericPackageDescription
  -- No version is specified, pick latest one
  lookupVersion m | [] <- version  = fmap snd . listToMaybe $ DB.toDescList m
  lookupVersion m = DB.lookup (DB.makeVersion version) m

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

sourceFromHackage :: Hash -> String -> String -> IO DerivationSource
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
    _       -> liftIO $ hPutStrLn stderr ("*** found more than one cabal file (" ++ show cabals ++ "). Exiting.") >> exitFailure

handleIO :: (Exception.IOException -> IO a) -> IO a -> IO a
handleIO = Exception.handle

hpackDirectory :: FilePath -> MaybeT IO (Bool, Cabal.GenericPackageDescription)
hpackDirectory dir = do
  mPackage <- liftIO $ Hpack.readPackageConfig $ dir </> "package.yaml"
  case mPackage of
    Left err -> liftIO $ hPutStrLn stderr ("*** hpack error: " ++ show err ++ ". Exiting.") >> exitFailure
    Right (_, pkg') -> do
      let hpackOutput = Hpack.renderPackage Hpack.defaultRenderSettings 2 [] [] pkg'
          buf = UTF8.fromString hpackOutput
          hash = printSHA256 $ digest (digestByName "sha256") buf
      case parsePackage' buf of
        Left msg -> liftIO $ do
          hPutStrLn stderr $ "*** cannot parse hpack output: " ++ msg
          hPutStrLn stderr $ "*** hpack output:\n" ++ hpackOutput
          hPutStrLn stderr "*** Exiting."
          exitFailure
        Right pkg -> MaybeT $ return $ Just $ (,) True $ pkg
          { packageDescription = (packageDescription pkg)
            { customFieldsPD = ("X-Cabal-File-Hash", hash) : customFieldsPD (packageDescription pkg)
          } }

cabalFromFile :: Bool -> FilePath -> MaybeT IO Cabal.GenericPackageDescription
cabalFromFile failHard file =
  -- readFile throws an error if it's used on binary files which contain sequences
  -- that do not represent valid characters. To catch that exception, we need to
  -- wrap the whole block in `catchIO`, because of lazy IO. The `case` will force
  -- the reading of the file, so we will always catch the expression here.
  MaybeT $ handleIO (\err -> Nothing <$ hPutStrLn stderr ("*** parsing cabal file: " ++ show err)) $ do
    buf <- LBS8.readFile file
    let hash = printSHA256 (digest (digestByName "sha256") buf)
    case parsePackage' buf of
      Left msg    -> if failHard
                     then fail ("*** cannot parse " ++ show file ++ ": " ++ msg)
                     else return Nothing
      Right pkg -> do return $ Just $ pkg { packageDescription = (packageDescription pkg) {
                                               customFieldsPD = ("X-Cabal-File-Hash", hash) : customFieldsPD (packageDescription pkg)
                                            }
                                          }
