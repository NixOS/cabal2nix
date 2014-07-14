module Cabal2Nix.Flags ( configureCabalFlags ) where

import Distribution.Package
import Distribution.PackageDescription

configureCabalFlags :: PackageIdentifier -> FlagAssignment
configureCabalFlags (PackageIdentifier (PackageName name) _)
 | name == "arithmoi"           = [disable "llvm"]
 | name == "accelerate-examples"= [disable "opencl"]
 | name == "git-annex"          = [ enable "S3", enable "WebDAV", enable "Inotify"
                                  , enable "Dbus", enable "Assistant", enable "Webapp"
                                  , enable "Pairing", enable "XMPP", enable "DNS"
                                  , enable "Production", enable "TDFA"]
 | name == "haskeline"          = [enable "terminfo"]
 | name == "hslua"              = [enable "system-lua"]
 | name == "idris"              = [enable "llvm", enable "gmp", enable "ffi"]
 | name == "io-streams"         = [enable "NoInteractiveTests"]
 | name == "reactive-banana-wx" = [disable "buildExamples"]
 | name == "snap-server"        = [enable "openssl"]
 | name == "xmobar"             = [enable "all_extensions"]
 | name == "xmonad-extras"      = [disable "with_hlist", enable "with_split", enable "with_parsec"]
 | name == "yi"                 = [enable "pango"]
 | otherwise                    = []

enable :: String -> (FlagName,Bool)
enable name = (FlagName name, True)

disable :: String -> (FlagName,Bool)
disable name = (FlagName name, False)
