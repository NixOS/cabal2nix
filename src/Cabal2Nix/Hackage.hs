module Cabal2Nix.Hackage ( hackagePath, Ext(..), hashPackage ) where

import Distribution.Package ( PackageIdentifier(..), PackageName(..) )
import System.Process ( readProcess )
import System.Directory ( doesFileExist, getHomeDirectory, createDirectoryIfMissing )
import System.FilePath ( dropFileName, (</>), (<.>) )
import Data.Version ( showVersion )
import Control.Monad ( when )

data Ext = TarGz | Cabal deriving Eq

showExt :: Ext -> String
showExt TarGz = ".tar.gz"
showExt Cabal = ".cabal"

hackagePath :: PackageIdentifier -> Ext -> String
hackagePath (PackageIdentifier (PackageName name) version') ext =
    "http://hackage.haskell.org/packages/archive/" ++
    name ++ "/" ++ version ++ "/" ++ name ++
    (if ext == TarGz then "-" ++ version else "") ++
    showExt ext
  where
    version = showVersion version'

hashPackage :: PackageIdentifier -> IO String
hashPackage pkg = do
    cachePath <- hashCachePath pkg
    exists <- doesFileExist cachePath
    hash' <- case exists of
              True -> readFile cachePath
              False -> readProcess "bash" ["-c", "exec nix-prefetch-url 2>/dev/tty " ++ hackagePath pkg TarGz] ""
    let hash = reverse (dropWhile (=='\n') (reverse hash'))
    when (not exists) $ do
      createDirectoryIfMissing True (dropFileName cachePath)
      writeFile cachePath hash
    return hash

hashCachePath :: PackageIdentifier -> IO FilePath
hashCachePath (PackageIdentifier (PackageName name) version') = do
    home <- getHomeDirectory
    return $ home ++ "/.cache/cabal2nix" </> name ++ "-" ++ version <.> "sha256"
  where
    version = showVersion version'
