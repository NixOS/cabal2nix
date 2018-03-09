{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Nixpkgs.Haskell.FromCabal.Flags ( configureCabalFlags ) where

import Distribution.Nixpkgs.Haskell.OrphanInstances ( )

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version

-- Cabal 2.1 compat: https://github.com/haskell/cabal/commit/6cb6a5165f19fd315bb5dc627cb8531d260d85f9#diff-67a944b99f4af1b1f86dd094973fea18L149
configureCabalFlags :: PackageIdentifier -> FlagAssignment
#if !MIN_VERSION_base(4,11,0)
configureCabalFlags = configureCabalFlags'
#else
configureCabalFlags = mkFlagAssignment . configureCabalFlags'
#endif

configureCabalFlags' :: PackageIdentifier -> [(FlagName, Bool)]
configureCabalFlags' (PackageIdentifier name version)
 | name == "accelerate-examples"= [disable "opencl"]
 | name == "arithmoi"           = [disable "llvm"]
 | name == "cabal-plan"         = [enable "exe"]
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
 | name == "hmatrix"            = [enable "openblas", enable "disable-default-paths"]
 | name == "hslua"              = [enable "system-lua"]
 | name == "idris"              = [enable "gmp", enable "ffi", enable "curses", ("execonly", version `withinRange` orLaterVersion (mkVersion [1,1,1])) ]
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
