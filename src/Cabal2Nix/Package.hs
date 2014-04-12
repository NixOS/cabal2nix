module Cabal2Nix.Package where

import Control.Exception ( handle, SomeException(..))
import Control.Monad
import Data.Functor ( (<$>) )
import Data.List ( isSuffixOf )
import Data.Maybe ( fromMaybe, fromJust )
import Distribution.NixOS.Derivation.Cabal
import Distribution.Text ( simpleParse )
import Network.Browser ( browse, request, setCheckForProxy, setDebugLog, setOutHandler )
import Network.HTTP ( getRequest, rspBody )
import System.Directory ( doesDirectoryExist, doesFileExist, createDirectoryIfMissing, getHomeDirectory, getDirectoryContents )
import System.Exit ( exitFailure )
import System.FilePath ( dropFileName, (</>), (<.>) )
import System.IO ( hPutStrLn, stderr)
import System.Process ( readProcess )
import Text.URI

import qualified Distribution.Hackage.DB as DB
import qualified Distribution.Package as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Parse as Cabal

data Package = Package
  { source :: DerivationSource
  , cabal :: Cabal.GenericPackageDescription
  }

readPackage :: Maybe String -> URI -> IO Package
readPackage optHash uri = case uriScheme uri of
  Nothing -> local
  Just "file" -> local
  Just "cabal" -> cabalPackage optHash $ fromMaybe "" (uriRegName uri) ++ uriPath uri
  Just scheme -> hPutStrLn stderr ("*** unsupported URI scheme: " ++ scheme) >> exitFailure
  where local = localPackage optHash $ fromMaybe "" (uriRegName uri) ++ uriPath uri

localPackage :: Maybe String -> FilePath -> IO Package
localPackage optHash path = do
  isDir <- doesDirectoryExist path
  cabalFile <- if isDir
    then do
      cabals <- filter (".cabal" `isSuffixOf`) <$> getDirectoryContents path
      case cabals of
        [] -> hPutStrLn stderr "*** No cabal file found in given directory" >> exitFailure
        (f:_) -> return $ path </> f
    else return path
  pkgDesc <- readFile cabalFile >>= readPackageDescription
  if isDir
    then return $ Package (Directory path) pkgDesc
    else do
      hash <- getHash optHash pkgDesc
      return $ Package (Hackage hash) pkgDesc

cabalPackage :: Maybe String -> String -> IO Package
cabalPackage optHash packageName = do
  pkgDesc <- case versionBranch $ Cabal.pkgVersion $ pid packageName of
    [] -> do
      packageDescription <- DB.lookup packageName `fmap` DB.readHackage
      case packageDescription of
        Just d -> return $ snd $ last $ DB.toList d
        Nothing -> error "No such package"
    _  -> fetchUrl (hackagePath (pid packageName) Cabal) >>= readPackageDescription
  hash <- getHash optHash pkgDesc
  return $ Package (Hackage hash) pkgDesc

  where pid p = fromJust $ simpleParse p

fetchUrl :: String -> IO String
fetchUrl url = do
  (_,rsp) <- browse $ do
     setCheckForProxy True
     setDebugLog Nothing
     setOutHandler (\_ -> return ())
     request (getRequest url)
  return (rspBody rsp)

readPackageDescription :: String -> IO Cabal.GenericPackageDescription
readPackageDescription source = case Cabal.parsePackageDescription source of
  Cabal.ParseOk _ a -> return a
  Cabal.ParseFailed err -> do
    hPutStrLn stderr ("*** cannot parse cabal file: " ++ show err)
    exitFailure

getHash :: Maybe String -> Cabal.GenericPackageDescription -> IO String
getHash optHash pkgDesc = maybe (hashPackage $ Cabal.package $ Cabal.packageDescription pkgDesc) return optHash

data Ext = TarGz | Cabal deriving Eq

showExt :: Ext -> String
showExt TarGz = ".tar.gz"
showExt Cabal = ".cabal"

hackagePath :: Cabal.PackageIdentifier -> Ext -> String
hackagePath (Cabal.PackageIdentifier (Cabal.PackageName name) version') ext =
    "http://hackage.haskell.org/packages/archive/" ++
    name ++ "/" ++ version ++ "/" ++ name ++
    (if ext == TarGz then '-' : version else "") ++
    showExt ext
  where
    version = showVersion version'

hashPackage :: Cabal.PackageIdentifier -> IO String
hashPackage pkg = do
    cachePath <- hashCachePath pkg
    exists <- doesFileExist cachePath
    hash' <- if exists
               then readFile cachePath
               else getHackageHash
    let hash = reverse (dropWhile (=='\n') (reverse hash'))
    unless exists $ do
      createDirectoryIfMissing True (dropFileName cachePath)
      writeFile cachePath hash
    return hash
  where getHackageHash = do
            let command = "exec nix-prefetch-url 2>/dev/tty " ++ hackagePath pkg TarGz
            handle handlePrefetchError (readProcess "bash" ["-c", command] "")
        handlePrefetchError (SomeException _) =
           error $ "\nError: Cannot compute hash. (Not a hackage project?)\n" ++
                  "Specify hash explicitly via --sha256 and add appropriate \"src\" attribute " ++
                  "to resulting nix expression."

hashCachePath :: Cabal.PackageIdentifier -> IO FilePath
hashCachePath (Cabal.PackageIdentifier (Cabal.PackageName name) version') = do
    home <- getHomeDirectory
    return $ home ++ "/.cache/cabal2nix" </> name ++ "-" ++ version <.> "sha256"
  where
    version = showVersion version'
