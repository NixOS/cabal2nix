{-# LANGUAGE OverloadedStrings #-}

module Distribution.Nixpkgs.Haskell.FromCabal.Flags ( configureCabalFlags ) where

import Distribution.Nixpkgs.Haskell.OrphanInstances ( )

import Data.Char
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version

configureCabalFlags :: PackageIdentifier -> FlagAssignment
configureCabalFlags = mkFlagAssignment . configureCabalFlags'

configureCabalFlags' :: PackageIdentifier -> [(FlagName,Bool)]
configureCabalFlags' (PackageIdentifier name version)
 | name == "accelerate-examples"= [disable "opencl"]
 | name == "arithmoi"           = [disable "llvm"]
 | name == "bustle"             = [disable "hgettext"] -- https://gitlab.freedesktop.org/bustle/bustle/issues/13
 | name == "cabal-plan"         = [enable "exe"]
 | name == "cassava"            = [disable "bytestring--lt-0_10_4"]
 | name == "cryptohash-sha256"  = [enable "use-cbits"] -- https://github.com/haskell-hvr/cryptohash-sha256/issues/11
 | name == "darcs"              = [enable "library", enable "force-char8-encoding"]
 | name == "diagrams-builder"   = [enable "cairo", enable "svg", enable "ps", enable "rasterific"]
 | name == "fltkhs"             = [enable "opengl"]
 | name == "folds"              = [disable "test-hlint"]
 | name == "git-annex"          = [ enable "assistant"
                                  , disable "benchmark"
                                  , enable "dbus"
                                  , disable "debuglocks"
                                  , enable "magicmime"
                                  , enable "networkbsd"
                                  , enable "pairing"
                                  , enable "production"
                                  , enable "s3"
                                  , enable "torrentparser"
                                  , enable "webapp"
                                  , enable "webdav"
                                  ]
 | name == "haskeline"          = [enable "terminfo"]
 | name == "haste-compiler"     = [enable "portable"]
 | name == "highlighting-kate"  = [enable "pcre-light"]
 | name == "hlibsass" && version >= mkVersion [0,1,5]
                                = [enable "externalLibsass"]
 | name == "hmatrix"            = [enable "openblas", enable "disable-default-paths"]
 | name == "hslua" && version < mkVersion [2,0,0]
                                = [enable "system-lua", disable "use-pkgconfig"]
 | name == "idris"              = [enable "gmp", enable "ffi", enable "curses", ("execonly", version `withinRange` orLaterVersion (mkVersion [1,1,1])) ]
 | name == "io-streams"         = [enable "NoInteractiveTests"]
 | name == "liquid-fixpoint"    = [enable "build-external"]
 | name == "lua" && version >= mkVersion [2,0,0]
                                = [enable "system-lua", disable "use-pkgconfig"]
 | name == "pandoc"             = [disable "trypandoc"]
 | name == "pandoc-placetable"  = [enable "inlineMarkdown"]
 | name == "persistent-sqlite"  = [enable "systemlib"]
 | name == "reactive-banana-wx" = [disable "buildExamples"]
 | name == "skylighting"        = [enable "executable"]
 | name == "snap-server"        = [enable "openssl"]
 | name == "stack"              = [enable "disable-git-info", enable "hide-dependency-versions", enable "supported-build"]
 | name == "xmobar"             = [enable "with_alsa", enable "with_conduit", enable "with_datezone", enable "with_dbus", enable "with_inotify", enable "with_iwlib", enable "with_mpd", enable "with_mpris", enable "with_rtsopts", enable "with_threaded", enable "with_utf8", enable "with_uvmeter", enable "with_weather", enable "with_xft", enable "with_xpm"]
 | name == "xmonad-extras"      = [disable "with_hlist", enable "with_split", enable "with_parsec"]
 | name == "yaml"               = [("system-libyaml", version >= "0.10.1.1")]
 | name == "yi"                 = [enable "pango", enable "vty"]
 | otherwise                    = []

enable :: String -> (FlagName,Bool)
enable name = (mkFlagName' name, True)

disable :: String -> (FlagName,Bool)
disable name = (mkFlagName' name, False)

mkFlagName' :: String -> FlagName
mkFlagName' = mkFlagName . map toLower
