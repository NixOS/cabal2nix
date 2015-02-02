{-# LANGUAGE RecordWildCards #-}

module Cabal2Nix.PostProcess ( postProcess ) where

import qualified Data.Set as Set
import Distribution.Nixpkgs.Haskell
import Distribution.Text ( display )

postProcess :: Derivation -> Derivation
postProcess = postProcess' . fixGtkBuilds

fixGtkBuilds :: Derivation -> Derivation
fixGtkBuilds deriv@(MkDerivation {..}) = deriv { pkgConfDeps = pkgConfDeps `Set.difference` buildDepends }

postProcess' :: Derivation -> Derivation
postProcess' deriv@(MkDerivation {..})
  | pname == "aeson" && version > Version [0,7] []
                                = deriv { buildDepends = Set.insert "blaze-builder" buildDepends }
  | pname == "Agda"             = deriv { buildTools = Set.insert "emacs" buildTools, phaseOverrides = agdaPostInstall }
  | pname == "alex" && version < Version [3,1] []
                                = deriv { buildTools = Set.insert "perl" buildTools }
  | pname == "alex" && version >= Version [3,1] []
                                = deriv { buildTools = Set.insert "perl" (Set.insert "happy" buildTools) }
  | pname == "apache-md5"       = deriv { testDepends = Set.delete "crypto" testDepends }
  | pname == "bindings-GLFW"    = deriv { extraLibs = Set.insert "libXext" (Set.insert "libXfixes" extraLibs) }
  | pname == "bits-extras"      = deriv { configureFlags = Set.insert "--ghc-option=-lgcc_s" configureFlags
                                        , extraLibs = Set.filter (/= "gcc_s") extraLibs
                                        }
  | pname == "Cabal"            = deriv { phaseOverrides = "preCheck = \"unset GHC_PACKAGE_PATH; export HOME=$NIX_BUILD_TOP\";" }
  | pname == "cabal-bounds"     = deriv { buildTools = Set.insert "cabal-install" buildTools }
  | pname == "cabal-install" && version >= Version [0,14] []
                                = deriv { phaseOverrides = cabalInstallPostInstall }
  | pname == "darcs"            = deriv { phaseOverrides = darcsInstallPostInstall }
  | pname == "dns"              = deriv { testTarget = "spec" }
  | pname == "editline"         = deriv { extraLibs = Set.insert "libedit" extraLibs }
  | pname == "epic"             = deriv { extraLibs = Set.insert "gmp" (Set.insert "boehmgc" extraLibs)
                                        , buildTools = Set.insert "happy" buildTools
                                        }
  | pname == "ghc-heap-view"    = deriv { phaseOverrides = ghciPostInstall }
  | pname == "ghc-mod"          = deriv { phaseOverrides = ghcModPostInstall pname version
                                        , buildTools = Set.insert "emacs" (Set.insert "makeWrapper" buildTools) }
  | pname == "ghc-parser"       = deriv { buildTools = Set.insert "cpphs" (Set.insert "happy" buildTools)
                                        , phaseOverrides = ghcParserPatchPhase }
  | pname == "ghc-paths"        = deriv { phaseOverrides = ghcPathsPatches }
  | pname == "ghc-vis"          = deriv { phaseOverrides = ghciPostInstall }
  | pname == "git-annex"        = deriv { phaseOverrides = gitAnnexOverrides
                                        , buildTools = Set.fromList ["git","rsync","gnupg1","curl","wget","lsof","openssh","which","bup","perl"] `Set.union` buildTools }
  | pname == "github-backup"    = deriv { buildTools = Set.insert "git" buildTools }
  | pname == "gloss-raster"     = deriv { extraLibs = Set.insert "llvm" extraLibs }
  | pname == "GLUT"             = deriv { extraLibs = Set.fromList ["glut","libSM","libICE","libXmu","libXi","mesa"] `Set.union` extraLibs }
  | pname == "gtkglext"         = deriv { pkgConfDeps = Set.insert "pangox_compat" pkgConfDeps }
  | pname == "gtk2hs-buildtools"= deriv { buildDepends = Set.insert "hashtables" buildDepends }
  | pname == "haddock" && version < Version [2,14] []
                                = deriv { buildTools = Set.insert "alex" (Set.insert "happy" buildTools) }
  | pname == "haddock"          = deriv { phaseOverrides = haddockPreCheck }
  | pname == "happy"            = deriv { buildTools = Set.insert "perl" buildTools }
  | pname == "haskeline"        = deriv { buildDepends = Set.insert "utf8-string" buildDepends }
  | pname == "haskell-src"      = deriv { buildTools = Set.insert "happy" buildTools }
  | pname == "haskell-src-meta" = deriv { buildDepends = Set.insert "uniplate" buildDepends }
  | pname == "HFuse"            = deriv { phaseOverrides = hfusePreConfigure }
  | pname == "highlighting-kate"= highlightingKatePostProcessing deriv
  | pname == "hlibgit2"         = deriv { buildTools = Set.insert "git" buildTools }
  | pname == "HList"            = deriv { buildTools = Set.insert "diffutils" buildTools }
  | pname == "hmatrix"          = deriv { extraLibs = Set.insert "liblapack" (Set.insert "blas" (Set.filter (/= "lapack") extraLibs)) }
  | pname == "hmatrix-special"  = deriv { extraLibs = Set.insert "gsl" extraLibs }
  | pname == "hoogle"           = deriv { testTarget = "--test-option=--no-net" }
  | pname == "hspec"            = deriv { doCheck = False }
  | pname == "GlomeVec"         = deriv { buildTools = Set.insert "llvm" buildTools }
  | pname == "idris"            = deriv { buildTools = Set.insert "happy" buildTools, extraLibs = Set.insert "gmp" (Set.insert "boehmgc" extraLibs) }
  | pname == "language-c-quote" = deriv { buildTools = Set.insert "alex" (Set.insert "happy" buildTools) }
  | pname == "language-java"    = deriv { buildDepends = Set.insert "syb" buildDepends }
  | pname == "leksah-server"    = deriv { buildDepends = Set.insert "process-leksah" buildDepends }
  | pname == "lhs2tex"          = deriv { extraLibs = Set.insert "texLive" extraLibs, phaseOverrides = lhs2texPostInstall }
  | pname == "libffi"           = deriv { extraLibs = Set.delete "ffi" extraLibs }
  | pname == "liquid-fixpoint"  = deriv { buildTools = Set.insert "ocaml" buildTools }
  | pname == "llvm-base"        = deriv { extraLibs = Set.insert "llvm" extraLibs }
  | pname == "llvm-general"     = deriv { doCheck = False }
  | pname == "llvm-general-pure"= deriv { doCheck = False }
  | pname == "MFlow"            = deriv { buildTools = Set.insert "cpphs" buildTools }
  | pname == "multiarg"         = deriv { buildDepends = Set.insert "utf8-string" buildDepends }
  | pname == "mysql"            = deriv { buildTools = Set.insert "mysqlConfig" buildTools, extraLibs = Set.insert "zlib" extraLibs }
  | pname == "ncurses"          = deriv { phaseOverrides = ncursesPatchPhase }
  | pname == "Omega"            = deriv { testDepends = Set.delete "stdc++" testDepends }
  | pname == "OpenAL"           = deriv { extraLibs = Set.insert "openal" extraLibs }
  | pname == "OpenGL"           = deriv { extraLibs = Set.insert "mesa" (Set.insert "libX11" extraLibs) }
  | pname == "pandoc"           = deriv { buildDepends = Set.insert "alex" (Set.insert "happy" buildDepends) }
  | pname == "pcap"             = deriv { extraLibs = Set.insert "libpcap" extraLibs }
  | pname == "persistent"       = deriv { extraLibs = Set.insert "sqlite3" extraLibs }
  | pname == "purescript"       = deriv { buildTools = Set.insert "nodejs" buildTools }
  | pname == "repa-algorithms"  = deriv { extraLibs = Set.insert "llvm" extraLibs }
  | pname == "readline"         = deriv { extraLibs = Set.insert "readline" (Set.insert "ncurses" extraLibs) }
  | pname == "repa-examples"    = deriv { extraLibs = Set.insert "llvm" extraLibs }
  | pname == "saltine"          = deriv { extraLibs = Set.map (\x -> if x == "sodium" then "libsodium" else x) extraLibs }
  | pname == "SDL-image"        = deriv { extraLibs = Set.insert "SDL_image" extraLibs }
  | pname == "SDL-mixer"        = deriv { extraLibs = Set.insert "SDL_mixer" extraLibs }
  | pname == "SDL-ttf"          = deriv { extraLibs = Set.insert "SDL_ttf" extraLibs }
  | pname == "sloane"           = deriv { phaseOverrides = sloanePostInstall }
  | pname == "structured-haskell-mode" = deriv { buildTools = Set.insert "emacs" buildTools
                                               , phaseOverrides = structuredHaskellModePostInstall
                                               }
  | pname == "terminfo"         = deriv { extraLibs = Set.insert "ncurses" extraLibs }
  | pname == "threadscope"      = deriv { configureFlags = Set.insert "--ghc-options=-rtsopts" configureFlags }
  | pname == "thyme"            = deriv { buildTools = Set.insert "cpphs" buildTools }
  | pname == "vacuum"           = deriv { extraLibs = Set.insert "ghc-paths" extraLibs }
  | pname == "wxc"              = deriv { extraLibs = Set.fromList ["wxGTK","mesa","libX11"] `Set.union` extraLibs
                                        , phaseOverrides = wxcPostInstall version
                                        }
  | pname == "wxcore"           = deriv { extraLibs = Set.fromList ["wxGTK","mesa","libX11"] `Set.union` extraLibs }
  | pname == "X11" && version >= Version [1,6] []
                                = deriv { extraLibs = Set.fromList ["libXinerama","libXext","libXrender"] `Set.union` extraLibs }
  | pname == "X11"              = deriv { extraLibs = Set.insert "libXinerama" (Set.insert "libXext" extraLibs) }
  | pname == "X11-xft"          = deriv { extraLibs = Set.fromList ["pkgconfig","freetype","fontconfig"] `Set.union` extraLibs
                                        , configureFlags = Set.insert "--extra-include-dirs=${freetype}/include/freetype2" configureFlags
                                        }
  | pname == "xmonad"           = deriv { phaseOverrides = xmonadPostInstall }

-- Unbreak packages during hackage2nix generation:

  | pname == "hnetcdf"          = deriv { testDepends = Set.delete "netcdf" testDepends }
  | pname == "SDL2-ttf"         = deriv { buildDepends = Set.delete "SDL2" buildDepends }
  | pname == "jsaddle"          = deriv { buildDepends = Set.delete "ghcjs-base" buildDepends, testDepends = Set.delete "ghcjs-base" testDepends }
  | pname == "hzk"              = deriv { testDepends = Set.delete "zookeeper_mt" testDepends, buildTools = Set.insert "zookeeper_mt" buildTools }
  | pname == "zip-archive"      = deriv { testDepends = Set.delete "zip" testDepends, buildTools = Set.insert "zip" buildTools }
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
  , buildDepends = Set.insert "regex-pcre" (Set.delete "regex-pcre-builtin" buildDepends)
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

haddockPreCheck :: String
haddockPreCheck = "preCheck = \"unset GHC_PACKAGE_PATH\";"

ghcParserPatchPhase :: String
ghcParserPatchPhase = unlines
  [ "patchPhase = ''"
  , "  substituteInPlace build-parser.sh --replace \"/bin/bash\" \"$SHELL\""
  , "'';"
  ]
