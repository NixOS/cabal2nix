{-# LANGUAGE OverloadedStrings #-}

module Distribution.Nixpkgs.Haskell.FromCabal.Flags ( configureCabalFlags ) where

import Distribution.Nixpkgs.Haskell.OrphanInstances ( )

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version

configureCabalFlags :: PackageIdentifier -> FlagAssignment
configureCabalFlags = mkFlagAssignment . configureCabalFlags'

configureCabalFlags' :: PackageIdentifier -> [(FlagName, Bool)]
configureCabalFlags' (PackageIdentifier name version)
 | name == "accelerate-examples"= [disable "opencl"]
 | name == "arithmoi"           = [disable "llvm"]
 | name == "cassava"            = [disable "bytestring--lt-0_10_4"]
 | name == "darcs"              = [enable "library", enable "force-char8-encoding"]
 | name == "diagrams-builder"   = [enable "cairo", enable "svg", enable "ps", enable "rasterific"]
 | name == "folds"              = [disable "test-hlint"]
 | name == "git-annex"          = [ enable "assistant"
                                  , enable "cryptonite"
                                  , enable "dbus"
                                  , enable "desktopnotify"
                                  , enable "dns"
                                  , enable "feed"
                                  , enable "inotify"
                                  , enable "pairing"
                                  , enable "production"
                                  , enable "quvi"
                                  , enable "s3"
                                  , enable "tahoe"
                                  , enable "tdfa"
                                  , ("testsuite", version `withinRange` "< 6.20170925 || >= 6.20171214")
                                  , enable "torrentparser"
                                  , enable "webapp"
                                  , enable "webapp-secure"
                                  , enable "webdav"
                                  , enable "xmpp"
                                  ]
 | name == "haskeline"          = [enable "terminfo"]
 | name == "haste-compiler"     = [enable "portable"]
 | name == "highlighting-kate"  = [enable "pcre-light"]
 | name == "hlibsass" && version >= mkVersion [0,1,5]
                                = [enable "externalLibsass"]
 | name == "hmatrix"            = [enable "openblas"]
 | name == "hslua"              = [enable "system-lua"]
 | name == "idris"              = [enable "gmp", enable "ffi", enable "curses", ("execonly", version `withinRange` (orLaterVersion (mkVersion [1,1,1]))) ]
 | name == "io-streams"         = [enable "NoInteractiveTests"]
 | name == "liquid-fixpoint"    = [enable "build-external"]
 | name == "pandoc"             = [enable "https", disable "trypandoc"]
 | name == "reactive-banana-wx" = [disable "buildExamples"]
 | name == "snap-server"        = [enable "openssl"]
 | name == "xmobar"             = [enable "all_extensions"]
 | name == "xmonad-extras"      = [disable "with_hlist", enable "with_split", enable "with_parsec"]
 | name == "yaml"               = [enable "system-libyaml"]
 | name == "yi"                 = [enable "pango", enable "vty"]
 | otherwise                    = []

enable :: String -> (FlagName,Bool)
enable name = (mkFlagName name, True)

disable :: String -> (FlagName,Bool)
disable name = (mkFlagName name, False)
