{-# LANGUAGE RecordWildCards #-}

module Cabal2Nix.PostProcess ( postProcess ) where

import Data.List
import Distribution.NixOS.Derivation.Cabal
import Distribution.Text ( display )

postProcess :: Derivation -> Derivation
postProcess deriv@(MkDerivation {..})
  | pname == "aeson" && version > Version [0,7] []
                                = deriv { buildDepends = "blaze-builder":buildDepends }
  | pname == "Agda"             = deriv { buildTools = "emacs":buildTools, phaseOverrides = agdaPostInstall }
  | pname == "alex" && version < Version [3,1] []
                                = deriv { buildTools = "perl":buildTools }
  | pname == "alex" && version >= Version [3,1] []
                                = deriv { buildTools = "perl":"happy":buildTools }
  | pname == "apache-md5"       = deriv { testDepends = delete "crypto" testDepends }
  | pname == "bindings-GLFW"    = deriv { extraLibs = "libXext":"libXfixes":extraLibs }
  | pname == "bits-extras"      = deriv { configureFlags = "--ghc-option=-lgcc_s":configureFlags, extraLibs = filter (/= "gcc_s") extraLibs }
  | pname == "Cabal"            = deriv { phaseOverrides = "preCheck = \"unset GHC_PACKAGE_PATH; export HOME=$NIX_BUILD_TOP\";" }
  | pname == "cabal-bounds"     = deriv { buildTools = "cabal-install":buildTools }
  | pname == "cabal-install" && version >= Version [0,14] []
                                = deriv { phaseOverrides = cabalInstallPostInstall }
  | pname == "darcs"            = deriv { phaseOverrides = darcsInstallPostInstall }
  | pname == "dns"              = deriv { testTarget = "spec" }
  | pname == "editline"         = deriv { extraLibs = "libedit":extraLibs }
  | pname == "epic"             = deriv { extraLibs = "gmp":"boehmgc":extraLibs, buildTools = "happy":buildTools }
  | pname == "ghc-heap-view"    = deriv { phaseOverrides = ghciPostInstall }
  | pname == "ghc-mod"          = deriv { phaseOverrides = ghcModPostInstall pname version, buildTools = "emacs":"makeWrapper":buildTools }
  | pname == "ghc-parser"       = deriv { buildTools = "cpphs":"happy":buildTools, phaseOverrides = ghcParserPatchPhase }
  | pname == "ghc-paths"        = deriv { phaseOverrides = ghcPathsPatches }
  | pname == "ghc-vis"          = deriv { phaseOverrides = ghciPostInstall }
  | pname == "git-annex"        = deriv { phaseOverrides = gitAnnexOverrides, buildTools = "git":"rsync":"gnupg1":"curl":"wget":"lsof":"openssh":"which":"bup":"perl":buildTools }
  | pname == "github-backup"    = deriv { buildTools = "git":buildTools }
  | pname == "gloss-raster"     = deriv { extraLibs = "llvm":extraLibs }
  | pname == "GLUT"             = deriv { extraLibs = "glut":"libSM":"libICE":"libXmu":"libXi":"mesa":extraLibs }
  | pname == "gtkglext"         = deriv { pkgConfDeps = "pangox_compat":pkgConfDeps }
  | pname == "gtk2hs-buildtools"= deriv { buildDepends = "hashtables":buildDepends }
  | pname == "haddock" && version < Version [2,14] []
                                = deriv { buildTools = "alex":"happy":buildTools }
  | pname == "haddock"          = deriv { phaseOverrides = haddockPreCheck }
  | pname == "happy"            = deriv { buildTools = "perl":buildTools }
  | pname == "haskeline"        = deriv { buildDepends = "utf8-string":buildDepends }
  | pname == "haskell-src"      = deriv { buildTools = "happy":buildTools }
  | pname == "haskell-src-meta" = deriv { buildDepends = "uniplate":buildDepends }
  | pname == "HFuse"            = deriv { phaseOverrides = hfusePreConfigure }
  | pname == "highlighting-kate"= highlightingKatePostProcessing deriv
  | pname == "hlibgit2"         = deriv { buildTools = "git":buildTools }
  | pname == "HList"            = deriv { buildTools = "diffutils":buildTools }
  | pname == "hmatrix"          = deriv { extraLibs = "liblapack":"blas": filter (/= "lapack") extraLibs }
  | pname == "hmatrix-special"  = deriv { extraLibs = "gsl":extraLibs }
  | pname == "hoogle"           = deriv { testTarget = "--test-option=--no-net" }
  | pname == "hspec"            = deriv { doCheck = False }
  | pname == "GlomeVec"         = deriv { buildTools = "llvm":buildTools }
  | pname == "idris"            = deriv { buildTools = "happy":buildTools, extraLibs = "gmp":"boehmgc":extraLibs }
  | pname == "language-c-quote" = deriv { buildTools = "alex":"happy":buildTools }
  | pname == "language-java"    = deriv { buildDepends = "syb":buildDepends }
  | pname == "leksah-server"    = deriv { buildDepends = "process-leksah":buildDepends }
  | pname == "lhs2tex"          = deriv { extraLibs = "texLive":extraLibs, phaseOverrides = lhs2texPostInstall }
  | pname == "libffi"           = deriv { extraLibs = delete "ffi" extraLibs }
  | pname == "liquid-fixpoint"  = deriv { buildTools = "ocaml":buildTools }
  | pname == "llvm-base"        = deriv { extraLibs = "llvm":extraLibs }
  | pname == "llvm-general"     = deriv { doCheck = False }
  | pname == "llvm-general-pure"= deriv { doCheck = False }
  | pname == "MFlow"            = deriv { buildTools = "cpphs":buildTools }
  | pname == "multiarg"         = deriv { buildDepends = "utf8-string":buildDepends }
  | pname == "mime-mail"        = deriv { extraFunctionArgs = ["sendmail ? \"sendmail\""], phaseOverrides = mimeMailConfigureFlags }
  | pname == "mysql"            = deriv { buildTools = "mysqlConfig":buildTools, extraLibs = "zlib":extraLibs }
  | pname == "ncurses"          = deriv { phaseOverrides = ncursesPatchPhase }
  | pname == "Omega"            = deriv { testDepends = delete "stdc++" testDepends }
  | pname == "OpenAL"           = deriv { extraLibs = "openal":extraLibs }
  | pname == "OpenGL"           = deriv { extraLibs = "mesa":"libX11":extraLibs }
  | pname == "pandoc"           = deriv { buildDepends = "alex":"happy":buildDepends }
  | pname == "pcap"             = deriv { extraLibs = "libpcap":extraLibs }
  | pname == "persistent"       = deriv { extraLibs = "sqlite3":extraLibs }
  | pname == "purescript"       = deriv { buildTools = "nodejs":buildTools }
  | pname == "repa-algorithms"  = deriv { extraLibs = "llvm":extraLibs }
  | pname == "repa-examples"    = deriv { extraLibs = "llvm":extraLibs }
  | pname == "saltine"          = deriv { extraLibs = map (\x -> if x == "sodium" then "libsodium" else x) extraLibs }
  | pname == "SDL-image"        = deriv { extraLibs = "SDL_image":extraLibs }
  | pname == "SDL-mixer"        = deriv { extraLibs = "SDL_mixer":extraLibs }
  | pname == "SDL-ttf"          = deriv { extraLibs = "SDL_ttf":extraLibs }
  | pname == "sloane"           = deriv { phaseOverrides = sloanePostInstall }
  | pname == "structured-haskell-mode" = deriv { buildTools = "emacs":buildTools, phaseOverrides = structuredHaskellModePostInstall }
  | pname == "terminfo"         = deriv { extraLibs = "ncurses":extraLibs }
  | pname == "threadscope"      = deriv { configureFlags = "--ghc-options=-rtsopts":configureFlags }
  | pname == "thyme"            = deriv { buildTools = "cpphs":buildTools }
  | pname == "tz"               = deriv { extraFunctionArgs = ["tzdata"], phaseOverrides = "preConfigure = \"export TZDIR=${tzdata}/share/zoneinfo\";" }
  | pname == "vacuum"           = deriv { extraLibs = "ghc-paths":extraLibs }
  | pname == "wxc"              = deriv { extraLibs = "wxGTK":"mesa":"libX11":extraLibs, phaseOverrides = wxcPostInstall version }
  | pname == "wxcore"           = deriv { extraLibs = "wxGTK":"mesa":"libX11":extraLibs }
  | pname == "X11" && version >= Version [1,6] []
                                = deriv { extraLibs = "libXinerama":"libXext":"libXrender":extraLibs }
  | pname == "X11"              = deriv { extraLibs = "libXinerama":"libXext":extraLibs }
  | pname == "X11-xft"          = deriv { extraLibs = "pkgconfig":"freetype":"fontconfig":extraLibs
                                        , configureFlags = "--extra-include-dirs=${freetype}/include/freetype2":configureFlags
                                        }
  | pname == "xmonad"           = deriv { phaseOverrides = xmonadPostInstall }

-- Unbreak packages during hackage2nix generation:

  | pname == "hnetcdf"          = deriv { testDepends = delete "netcdf" testDepends }
  | pname == "SDL2-ttf"         = deriv { buildDepends = delete "SDL2" buildDepends }
  | pname == "jsaddle"          = deriv { buildDepends = delete "ghcjs-base" buildDepends, testDepends = delete "ghcjs-base" testDepends }
  | pname == "hzk"              = deriv { testDepends = delete "zookeeper_mt" testDepends, buildTools = "zookeeper_mt":buildTools }
  | pname == "zip-archive"      = deriv { testDepends = delete "zip" testDepends, buildTools = "zip":buildTools }
  | otherwise                   = deriv

ghcModPostInstall :: String -> Version -> String
ghcModPostInstall pname version = unlines
  [ "configureFlags = \"--datasubdir=" ++ pname ++ "-" ++ display version ++ "\";"
  , "postInstall = ''"
  , "  cd $out/share/" ++ pname ++ "-" ++ display version
  , "  make"
  , "  rm Makefile"
  , "  cd .."
  , "  ensureDir \"$out/share/emacs\""
  , "  mv " ++ pname ++ "-" ++ display version ++ " emacs/site-lisp"
  , "'';"
  ]

wxcPostInstall :: Version -> String
wxcPostInstall version = unlines
  [ "postInstall = ''"
  , "  cp -v dist/build/libwxc.so." ++ display version ++ " $out/lib/libwxc.so"
  , "'';"
  ]

cabalInstallPostInstall :: String
cabalInstallPostInstall = unlines
  [ "postInstall = ''"
  , "  mkdir $out/etc"
  , "  mv bash-completion $out/etc/bash_completion.d"
  , "'';"
  ]

darcsInstallPostInstall :: String
darcsInstallPostInstall = unlines
  [ "postInstall = ''"
  , "  mkdir -p $out/etc/bash_completion.d"
  , "  mv contrib/darcs_completion $out/etc/bash_completion.d/darcs"
  , "'';"
  ]

highlightingKatePostProcessing :: Derivation -> Derivation
highlightingKatePostProcessing deriv@(MkDerivation {..}) = deriv
  { phaseOverrides = "prePatch = \"sed -i -e 's|regex-pcre-builtin >= .*|regex-pcre|' highlighting-kate.cabal\";"
  , buildDepends = "regex-pcre" : filter (/="regex-pcre-builtin") buildDepends
  }

xmonadPostInstall :: String
xmonadPostInstall = unlines
  [ "postInstall = ''"
  , "  shopt -s globstar"
  , "  mkdir -p $out/share/man/man1"
  , "  mv \"$out/\"**\"/man/\"*.1 $out/share/man/man1/"
  , "'';"
  ]

gitAnnexOverrides :: String
gitAnnexOverrides = unlines
  [ "preConfigure = \"export HOME=$TEMPDIR\";"
  , "checkPhase = ''"
  , "  cp dist/build/git-annex/git-annex git-annex"
  , "  ./git-annex test"
  , "'';"
  ]

ghciPostInstall :: String
ghciPostInstall = unlines
  [ "postInstall = ''"
  , "  ensureDir \"$out/share/ghci\""
  , "  ln -s \"$out/share/$pname-$version/ghci\" \"$out/share/ghci/$pname\""
  , "'';"
  ]

hfusePreConfigure :: String
hfusePreConfigure = unlines
  [ "preConfigure = ''"
  , "  sed -i -e \"s@  Extra-Lib-Dirs:         /usr/local/lib@  Extra-Lib-Dirs:         ${fuse}/lib@\" HFuse.cabal"
  , "'';"
  ]

ghcPathsPatches :: String
ghcPathsPatches = "patches = [ ./ghc-paths-nix.patch ];"

lhs2texPostInstall :: String
lhs2texPostInstall = unlines
  [ "postInstall = ''"
  , "  mkdir -p \"$out/share/doc/$name\""
  , "  cp doc/Guide2.pdf $out/share/doc/$name"
  , "  mkdir -p \"$out/nix-support\""
  , "'';"
  ]

ncursesPatchPhase :: String
ncursesPatchPhase = "patchPhase = \"find . -type f -exec sed -i -e 's|ncursesw/||' {} \\\\;\";"

agdaPostInstall :: String
agdaPostInstall = unlines
  [ "postInstall = ''"
  , "  $out/bin/agda -c --no-main $(find $out/share -name Primitive.agda)"
  , "  $out/bin/agda-mode compile"
  , "'';"
  ]

structuredHaskellModePostInstall :: String
structuredHaskellModePostInstall = unlines
  [ "postInstall = ''"
  , "  emacs -L elisp --batch -f batch-byte-compile \"elisp/\"*.el"
  , "  install -d $out/share/emacs/site-lisp"
  , "  install \"elisp/\"*.el \"elisp/\"*.elc  $out/share/emacs/site-lisp"
  , "'';"
  ]

sloanePostInstall :: String
sloanePostInstall = unlines
  [ "postInstall = ''"
  , "  mkdir -p $out/share/man/man1"
  , "  cp sloane.1 $out/share/man/man1/"
  , "'';"
  ]

mimeMailConfigureFlags :: String
mimeMailConfigureFlags = unlines
  [ "configureFlags = \"--ghc-option=-DMIME_MAIL_SENDMAIL_PATH=\\\"${sendmail}\\\"\";"
  ]

haddockPreCheck :: String
haddockPreCheck = "preCheck = \"unset GHC_PACKAGE_PATH\";"

ghcParserPatchPhase :: String
ghcParserPatchPhase = unlines
  [ "patchPhase = ''"
  , "  substituteInPlace build-parser.sh --replace \"/bin/bash\" \"$SHELL\""
  , "'';"
  ]
