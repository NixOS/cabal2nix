module Cabal2Nix.Flags ( pkgConfigureFlags ) where

import Distribution.Package
import Distribution.PackageDescription

pkgConfigureFlags :: PackageIdentifier -> (FlagAssignment,[String])
pkgConfigureFlags (PackageIdentifier (PackageName name) _)
 | name == "pandoc"             = ([enable "highlighting", enable "threaded"],[])
 | name == "threadscope"        = ([], ["--ghc-options=-rtsopts"])
 | name == "X11-xft"            = ([], ["--extra-include-dirs=${freetype}/include/freetype2"])
 | name == "xmonad-extras"      = ([disable "with_hlist", disable "with_mpd"], [])
 | otherwise                    = ([],[])

enable :: String -> (FlagName,Bool)
enable name = (FlagName name, True)

disable :: String -> (FlagName,Bool)
disable name = (FlagName name, False)
