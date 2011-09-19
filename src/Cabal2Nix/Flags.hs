module Cabal2Nix.Flags ( configureCabalFlags ) where

import Distribution.Package
import Distribution.PackageDescription

configureCabalFlags :: PackageIdentifier -> FlagAssignment
configureCabalFlags (PackageIdentifier (PackageName name) _)
 | name == "pandoc"             = [enable "highlighting", enable "threaded"]
 | name == "xmonad-extras"      = [disable "with_hlist", disable "with_mpd"]
 | name == "xmobar"             = [enable "with_xft"]
 | otherwise                    = []

enable :: String -> (FlagName,Bool)
enable name = (FlagName name, True)

disable :: String -> (FlagName,Bool)
disable name = (FlagName name, False)
