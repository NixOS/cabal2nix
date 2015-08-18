module Cabal2Nix.Package where

import qualified Cabal2Nix.Hackage as DB
import qualified Control.Exception as Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.List ( isSuffixOf, isPrefixOf )
import Data.Maybe
import Data.Version
import Distribution.Nixpkgs.Fetch
import qualified Distribution.Package as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Parse as Cabal
import qualified Distribution.ParseUtils as ParseUtils
import Distribution.Text ( simpleParse )
import System.Directory ( doesDirectoryExist, doesFileExist, createDirectoryIfMissing, getHomeDirectory, getDirectoryContents )
import System.Exit ( exitFailure )
import System.FilePath ( (</>), (<.>) )
import System.IO ( hPutStrLn, stderr, hPutStr )

data Package = Package
  { pkgSource :: DerivationSource
  , pkgCabal :: Cabal.GenericPackageDescription
  }
  deriving (Show)

getPackage :: Maybe String -> Source -> IO Package
getPackage optHackageDB source = do
  (derivSource, pkgDesc) <- fetchOrFromDB optHackageDB source
  flip Package pkgDesc <$> maybe (sourceFromHackage (sourceHash source) $ showPackageIdentifier pkgDesc) return derivSource


fetchOrFromDB :: Maybe String -> Source -> IO (Maybe DerivationSource, Cabal.GenericPackageDescription)
fetchOrFromDB optHackageDB src
  | "cabal://" `isPrefixOf` sourceUrl src = fmap ((,) Nothing) . fromDB optHackageDB . drop (length "cabal://") $ sourceUrl src
  | otherwise                             = do
    r <- fetch cabalFromPath src
    case r of
      Nothing ->
        hPutStrLn stderr "*** failed to fetch source. Does the URL exist?" >> exitFailure
      Just (derivSource, (externalSource, pkgDesc)) ->
        return (derivSource <$ guard externalSource, pkgDesc)

fromDB :: Maybe String -> String -> IO Cabal.GenericPackageDescription
fromDB optHackageDB pkg = do
  pkgDesc <- (lookupVersion <=< DB.lookup name) <$> maybe DB.readHashedHackage DB.readHackage' optHackageDB
  case pkgDesc of
    Just r -> return r
    Nothing -> hPutStrLn stderr "*** no such package in the cabal database (did you run cabal update?). " >> exitFailure
 where
  pkgId = fromMaybe (error ("invalid Haskell package id " ++ show pkg)) (simpleParse pkg)
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

sourceFromHackageHash :: String -> String -> DerivationSource
sourceFromHackageHash hash pkgId = DerivationSource "url" url "" hash
  where url = "mirror://hackage/" ++ pkgId ++ ".tar.gz"

sourceFromHackage :: Hash -> String -> IO DerivationSource
sourceFromHackage optHash pkgId = do
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
      maybeHash <- runMaybeT (derivHash . fst <$> fetchWith (False, "url", []) (Source url "" UnknownHash))
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
showPackageIdentifier pkgDesc = name ++ "-" ++ showVersion version where
  pkgId = Cabal.package . Cabal.packageDescription $ pkgDesc
  Cabal.PackageName name = Cabal.packageName pkgId
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
    _       -> liftIO $ hPutStrLn stderr "*** found zero or more than one cabal file. Exiting." >> exitFailure

handleIO :: (Exception.IOException -> IO a) -> IO a -> IO a
handleIO = Exception.handle

cabalFromFile :: Bool -> FilePath -> MaybeT IO Cabal.GenericPackageDescription
cabalFromFile failHard file =
  -- readFile throws an error if it's used on binary files which contain sequences
  -- that do not represent valid characters. To catch that exception, we need to
  -- wrap the whole block in `catchIO`, because of lazy IO. The `case` will force
  -- the reading of the file, so we will always catch the expression here.
  MaybeT $ handleIO (\err -> Nothing <$ hPutStrLn stderr ("*** parsing cabal file: " ++ show err)) $ do
    content <- readFile file
    case Cabal.parsePackageDescription content of
      Cabal.ParseFailed e | failHard -> do
        let (line, err) = ParseUtils.locatedErrorMsg e
            msg = maybe "" ((++ ": ") . show) line ++ err
        hPutStrLn stderr $ "*** error parsing cabal file: " ++ msg
        exitFailure
      Cabal.ParseFailed _  -> return Nothing
      Cabal.ParseOk     _ a -> return (Just a)
