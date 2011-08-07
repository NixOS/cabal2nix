module Cabal2Nix.Hackage ( hackagePath, Ext(..), hashPackage ) where

import Distribution.Package ( PackageIdentifier(..), PackageName(..) )
import System.Process ( readProcess )
import Data.Version ( showVersion )

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
    hash <- readProcess "bash" ["-c", "exec nix-prefetch-url 2>/dev/tty " ++ hackagePath pkg TarGz] ""
    return (reverse (dropWhile (=='\n') (reverse hash)))
