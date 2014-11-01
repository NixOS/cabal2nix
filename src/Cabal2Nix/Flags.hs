module Cabal2Nix.Flags ( configureCabalFlags ) where

import Distribution.Package
import Distribution.PackageDescription

configureCabalFlags :: PackageIdentifier -> FlagAssignment
configureCabalFlags (PackageIdentifier (PackageName name) _)
 | name == "arithmoi"           = [disable "llvm"]
 | name == "accelerate-examples"= [disable "opencl"]
 | name == "folds"              = [disable "test-hlint"]
 | name == "git-annex"          = [enable "Assistant" , enable "Production"]
 | name == "haskeline"          = [enable "terminfo"]
 | name == "haste-compiler"     = [enable "portable"]
 | name == "hslua"              = [enable "system-lua"]
 | name == "hxt"                = [enable "network-uri"]
 | name == "idris"              = [enable "gmp", enable "ffi"]
 | name == "io-streams"         = [enable "NoInteractiveTests"]
 | name == "pandoc"             = [enable "https", enable "make-pandoc-man-pages"]
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
