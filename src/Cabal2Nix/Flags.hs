module Cabal2Nix.Flags ( configureCabalFlags ) where

import Distribution.Package
import Distribution.PackageDescription

configureCabalFlags :: PackageIdentifier -> FlagAssignment
configureCabalFlags (PackageIdentifier (PackageName name) _)
 | name == "accelerate-examples"= [disable "opencl"]
 | name == "arithmoi"           = [disable "llvm"]
 | name == "darcs"              = [enable "library", enable "force-char8-encoding"]
 | name == "diagrams-builder"   = [enable "cairo", enable "svg", enable "ps", enable "rasterific"]
 | name == "folds"              = [disable "test-hlint"]
 | name == "git-annex"          = [enable "Assistant" , enable "Production"]
 | name == "haskeline"          = [enable "terminfo"]
 | name == "haste-compiler"     = [enable "portable"]
 | name == "highlighting-kate"  = [enable "pcre-light"]
 | name == "hslua"              = [enable "system-lua"]
 | name == "hxt"                = [enable "network-uri"]
 | name == "idris"              = [enable "gmp", enable "ffi"]
 | name == "io-streams"         = [enable "NoInteractiveTests"]
 | name == "pandoc"             = [enable "https"]
 | name == "reactive-banana-wx" = [disable "buildExamples"]
 | name == "snap-server"        = [enable "openssl"]
 | name == "xmobar"             = [enable "all_extensions"]
 | name == "xmonad-extras"      = [disable "with_hlist", enable "with_split", enable "with_parsec"]
 | name == "yi"                 = [enable "pango", enable "vty"]
 | otherwise                    = []

enable :: String -> (FlagName,Bool)
enable name = (FlagName name, True)

disable :: String -> (FlagName,Bool)
disable name = (FlagName name, False)
