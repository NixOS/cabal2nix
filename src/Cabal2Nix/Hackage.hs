module Cabal2Nix.Hackage ( hashPackage, readCabalFile ) where

import Control.Monad ( unless )
import Data.List ( isPrefixOf )
import Data.Maybe ( fromJust )
import Data.Version ( showVersion, versionBranch )
import Distribution.Package ( PackageIdentifier(..), PackageName(..) )
import Distribution.Text
import Network.HTTP ( getRequest, rspBody )
import Network.Browser ( browse, request, setCheckForProxy, setDebugLog, setOutHandler )
import System.Directory ( doesFileExist, getHomeDirectory, createDirectoryIfMissing )
import System.FilePath ( dropFileName, (</>), (<.>) )
import System.Process ( readProcess )
import Control.Exception (handle, SomeException(..))

import qualified Distribution.Hackage.DB as DB

data Ext = TarGz | Cabal deriving Eq

showExt :: Ext -> String
showExt TarGz = ".tar.gz"
showExt Cabal = ".cabal"

hackagePath :: PackageIdentifier -> Ext -> String
hackagePath (PackageIdentifier (PackageName name) version') ext =
    "http://hackage.haskell.org/packages/archive/" ++
    name ++ "/" ++ version ++ "/" ++ name ++
    (if ext == TarGz then '-' : version else "") ++
    showExt ext
  where
    version = showVersion version'

hashPackage :: PackageIdentifier -> IO String
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

hashCachePath :: PackageIdentifier -> IO FilePath
hashCachePath (PackageIdentifier (PackageName name) version') = do
    home <- getHomeDirectory
    return $ home ++ "/.cache/cabal2nix" </> name ++ "-" ++ version <.> "sha256"
  where
    version = showVersion version'

readCabalFile :: FilePath -> IO String
readCabalFile path
  | "cabal://" `isPrefixOf` path = do let pid p = fromJust $ simpleParse p
                                          packageName = drop 8 path
                                      case versionBranch $ pkgVersion $ pid packageName of
                                        [] -> do
                                          packageDescription <- DB.lookup packageName `fmap` DB.readHackage
                                          case packageDescription of
                                            Just d -> do
                                              let version = showVersion $ last $ DB.keys d
                                              readCabalFile $ hackagePath (pid $ packageName ++ "-" ++ version) Cabal
                                            Nothing -> error "No such package"
                                        _  -> readCabalFile $ hackagePath (pid packageName) Cabal
  | "http://"  `isPrefixOf` path = fetchUrl path
  | "file://"  `isPrefixOf` path = readCabalFile (drop 7 path)
  | otherwise                    = readFile path

fetchUrl :: String -> IO String
fetchUrl url = do
  (_,rsp) <- browse $ do
     setCheckForProxy True
     setDebugLog Nothing
     setOutHandler (\_ -> return ())
     request (getRequest url)
  return (rspBody rsp)
