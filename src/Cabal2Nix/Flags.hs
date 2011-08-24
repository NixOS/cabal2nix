module Cabal2Nix.Flags ( configureFlags ) where

import Distribution.Package
import Distribution.PackageDescription

configureFlags :: PackageIdentifier -> FlagAssignment
configureFlags (PackageIdentifier (PackageName name) _)
 | name == "pandoc"     = [enable "highlighting", enable "threaded"]
 | otherwise            = []

enable :: String -> (FlagName,Bool)
enable name = (FlagName name, True)

-- Uncommend this function when it's actually used.
--
-- disable :: String -> (FlagName,Bool)
-- disable name = (FlagName name, False)
