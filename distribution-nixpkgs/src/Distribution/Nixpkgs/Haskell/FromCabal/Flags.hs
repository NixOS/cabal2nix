module Distribution.Nixpkgs.Haskell.FromCabal.Flags ( configureCabalFlags ) where

import Distribution.Package
import Distribution.PackageDescription

configureCabalFlags :: PackageIdentifier -> FlagAssignment
configureCabalFlags (PackageIdentifier (PackageName name) _)
 | name == "accelerate-examples"= [disable "opencl"]
 | name == "arithmoi"           = [disable "llvm"]
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
                                  , enable "testsuite"
                                  , enable "torrentparser"
                                  , enable "webapp"
                                  , enable "webapp-secure"
                                  , enable "webdav"
                                  , enable "xmpp"
                                  ]
 | name == "haskeline"          = [enable "terminfo"]
 | name == "haste-compiler"     = [enable "portable"]
 | name == "highlighting-kate"  = [enable "pcre-light"]
 | name == "hlibsass"           = [enable "externalLibsass"]
 | name == "hmatrix"            = [enable "openblas"]
 | name == "hslua"              = [enable "system-lua"]
 | name == "idris"              = [enable "gmp", enable "ffi", enable "curses"]
 | name == "io-streams"         = [enable "NoInteractiveTests"]
 | name == "liquid-fixpoint"    = [enable "build-external"]
 | name == "pandoc"             = [enable "https"]
 | name == "reactive-banana-wx" = [disable "buildExamples"]
 | name == "snap-server"        = [enable "openssl"]
 | name == "xmobar"             = [enable "all_extensions"]
 | name == "xmonad-extras"      = [disable "with_hlist", enable "with_split", enable "with_parsec"]
 | name == "yaml"               = [enable "system-libyaml"]
 | name == "yi"                 = [enable "pango", enable "vty"]
 | otherwise                    = []

enable :: String -> (FlagName,Bool)
enable name = (FlagName name, True)

disable :: String -> (FlagName,Bool)
disable name = (FlagName name, False)
