module Cabal2Nix.Flags ( configureFlags ) where

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration

configureFlags :: PackageIdentifier -> FlagAssignment
configureFlags (PackageIdentifier (PackageName name) version)
 | name == "pandoc"     = [enable "highlighting", enable "threaded"]
 | otherwise            = []


enable :: String -> (FlagName,Bool)
enable name = (FlagName name, True)

disable :: String -> (FlagName,Bool)
disable name = (FlagName name, False)
