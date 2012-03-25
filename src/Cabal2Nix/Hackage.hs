module Cabal2Nix.Hackage ( hashPackage, readCabalFile ) where

import Control.Monad ( unless )
import Data.List ( isPrefixOf )
import Data.Version ( showVersion )
import Distribution.Package ( PackageIdentifier(..), PackageName(..) )
import Distribution.Text
import Network.HTTP ( getRequest, rspBody )
import Network.Browser ( browse, request, setCheckForProxy, setDebugLog, setOutHandler )
import System.Directory ( doesFileExist, getHomeDirectory, createDirectoryIfMissing )
import System.FilePath ( dropFileName, (</>), (<.>) )
import System.Process ( readProcess )

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
               else readProcess "bash" ["-c", "exec nix-prefetch-url 2>/dev/tty " ++ hackagePath pkg TarGz] ""
    let hash = reverse (dropWhile (=='\n') (reverse hash'))
    unless exists $ do
      createDirectoryIfMissing True (dropFileName cachePath)
      writeFile cachePath hash
    return hash

hashCachePath :: PackageIdentifier -> IO FilePath
hashCachePath (PackageIdentifier (PackageName name) version') = do
    home <- getHomeDirectory
    return $ home ++ "/.cache/cabal2nix" </> name ++ "-" ++ version <.> "sha256"
  where
    version = showVersion version'

readCabalFile :: FilePath -> IO String
readCabalFile path
  | "cabal://" `isPrefixOf` path = let Just pid = simpleParse (drop 8 path) in readCabalFile (hackagePath pid Cabal)
  | "http://"  `isPrefixOf` path = fetchUrl path
  | "file://"  `isPrefixOf` path = readCabalFile (drop 7 path)
  | otherwise                    = readFile path

fetchUrl :: String -> IO String
fetchUrl url = do
  (_,rsp) <- Network.Browser.browse $ do
     setCheckForProxy True
     setDebugLog Nothing
     setOutHandler (\_ -> return ())
     request (getRequest url)
  return (rspBody rsp)
