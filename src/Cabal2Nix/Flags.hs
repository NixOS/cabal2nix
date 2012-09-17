module Cabal2Nix.Flags ( configureCabalFlags ) where

import Distribution.Package
import Distribution.PackageDescription

configureCabalFlags :: PackageIdentifier -> FlagAssignment
configureCabalFlags (PackageIdentifier (PackageName name) _)
 | name == "accelerate-examples"= [disable "opencl"]
 | name == "pandoc"             = [enable "blaze_html_0_5"]
 | name == "haskeline"          = [enable "terminfo"]
 | name == "reactive-banana-wx" = [disable "buildExamples"]
 | name == "xmobar"             = [enable "with_xft", enable "with_iwlib"]
 | name == "xmonad-extras"      = [disable "with_hlist"]
 | otherwise                    = []

enable :: String -> (FlagName,Bool)
enable name = (FlagName name, True)

disable :: String -> (FlagName,Bool)
disable name = (FlagName name, False)
