module Cabal2Nix.PostProcess ( postProcess ) where

import Control.Lens
import Data.Maybe
import qualified Data.Set as Set
import Distribution.Nixpkgs.Haskell
import Distribution.Package
import Distribution.Text
import Distribution.Version

postProcess :: Derivation -> Derivation
postProcess deriv = foldr ($) (fixGtkBuilds deriv) [ f | (Dependency n vr, f) <- hooks, packageName deriv == n, packageVersion deriv `withinRange` vr ]

fixGtkBuilds :: Derivation -> Derivation
fixGtkBuilds drv = drv & dependencies . pkgconfig %~ (`Set.difference` buildDeps)
  where
    buildDeps = drv ^. dependencies . haskell

hooks :: [(Dependency, Derivation -> Derivation)]
hooks = over (mapped._1) (\str -> fromMaybe (error ("invalid constraint: " ++ show str)) (simpleParse str))
  [ ("dns", set testTarget "spec")      -- don't execute tests that try to access the network
  , ("bindings-GLFW", over (libraryDepends . system) (Set.union (Set.fromList [dep "libXext", dep "libXfixes"])))
  , ("cabal-install", set phaseOverrides cabalInstallPostInstall)
  , ("darcs", set phaseOverrides darcsInstallPostInstall)
  , ("git-annex", gitAnnexHook)
  , ("haddock", set phaseOverrides "preCheck = \"unset GHC_PACKAGE_PATH\";")
  , ("HFuse", set phaseOverrides hfusePreConfigure)
  , ("gf", set phaseOverrides gfPhaseOverrides . set doCheck False)
  , ("GlomeVec", set (libraryDepends . pkgconfig . contains (dep "llvm")) True)
  , ("gtk3", set (libraryDepends . pkgconfig . contains (dep "gtk3")) True)
  , ("jsaddle", set (dependencies . haskell . contains (dep "ghcjs-base")) False)
  , ("mysql", set (libraryDepends . system . contains (dep "mysql")) True)
  , ("pango", set (libraryDepends . haskell . contains (dep "cairo")) True)
  , ("readline", over (libraryDepends . system) (Set.union (Set.fromList [dep "readline", dep "ncurses"])))
  , ("monad", set phaseOverrides xmonadPostInstall)
  , ("wxc", wxcHook)
  , ("wxcore", set (libraryDepends . pkgconfig . contains (dep "wxGTK")) True)
  ]

dep :: String -> Dependency
dep s = Dependency (PackageName s) anyVersion

gitAnnexHook :: Derivation -> Derivation
gitAnnexHook = set phaseOverrides gitAnnexOverrides . over (executableDepends . system) (Set.union buildInputs)
  where
    gitAnnexOverrides = unlines
      [ "preConfigure = \"export HOME=$TEMPDIR\";"
      , "checkPhase = ''"
      , "  cp dist/build/git-annex/git-annex git-annex"
      , "  ./git-annex test"
      , "'';"
      ]
    buildInputs = Set.map dep $ Set.fromList ["git","rsync","gnupg","curl","wget","lsof","openssh","which","bup","perl"]

hfusePreConfigure :: String
hfusePreConfigure = unlines
  [ "preConfigure = ''"
  , "  sed -i -e \"s@  Extra-Lib-Dirs:         /usr/local/lib@  Extra-Lib-Dirs:         ${fuse}/lib@\" HFuse.cabal"
  , "'';"
  ]

gfPhaseOverrides :: String
gfPhaseOverrides = unlines
  [ "postPatch = ''"
  , "  sed -i \"s|\\\"-s\\\"|\\\"\\\"|\" ./Setup.hs"
    -- Disable silent compilation. Compiling takes long, it is best to see some
    -- output, otherwise it looks like the build step has stalled.
  , "  sed -i \"s|numJobs (bf bi)++||\" ./Setup.hs"
    -- Parallel compilation fails. Disable it.
  , "'';"
  , "preBuild = ''export LD_LIBRARY_PATH=`pwd`/dist/build:$LD_LIBRARY_PATH'';"
    -- The build step itself, after having built the library, needs to be able
    -- to find the library it just built in order to compile grammar files.
  ]

wxcHook :: Derivation -> Derivation
wxcHook drv = drv & libraryDepends . system %~ Set.union (Set.fromList [dep "mesa", dep "libX11"])
                  & libraryDepends . pkgconfig . contains (dep "wxGTK") .~ True
                  & phaseOverrides .~ wxcPostInstall (packageVersion drv)
                  & runHaddock .~ False
  where
    wxcPostInstall :: Version -> String
    wxcPostInstall version = unlines
      [ "postInstall = \"cp -v dist/build/libwxc.so." ++ display version ++ " $out/lib/libwxc.so\";"
      , "postPatch = \"sed -i -e '/ldconfig inst_lib_dir/d' Setup.hs\";"
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

xmonadPostInstall :: String
xmonadPostInstall = unlines
  [ "postInstall = ''"
  , "  shopt -s globstar"
  , "  mkdir -p $out/share/man/man1"
  , "  mv \"$out/\"**\"/man/\"*.1 $out/share/man/man1/"
  , "'';"
  ]

{-
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
  | pname == "bits-extras"      = deriv { configureFlags = Set.insert "--ghc-option=-lgcc_s" configureFlags
                                        , extraLibs = Set.filter (/= "gcc_s") extraLibs
                                        }
  | pname == "Cabal"            = deriv { phaseOverrides = "preCheck = \"unset GHC_PACKAGE_PATH; export HOME=$NIX_BUILD_TOP\";" }
  | pname == "cabal-bounds"     = deriv { buildTools = Set.insert "cabal-install" buildTools }
  | pname == "editline"         = deriv { extraLibs = Set.insert "libedit" extraLibs }
  | pname == "epic"             = deriv { extraLibs = Set.insert "gmp" (Set.insert "boehmgc" extraLibs)
                                        , buildTools = Set.insert "happy" buildTools
                                        }
  | pname == "ghc-heap-view"    = deriv { phaseOverrides = ghciPostInstall }
  | pname == "ghc-mod"          = deriv { phaseOverrides = ghcModPostInstall pname version, buildTools = Set.insert "emacs" buildTools }
  | pname == "ghc-parser"       = deriv { buildTools = Set.insert "cpphs" (Set.insert "happy" buildTools)
                                        , phaseOverrides = ghcParserPatchPhase }
  | pname == "ghc-paths"        = deriv { phaseOverrides = ghcPathsPatches }
  | pname == "ghc-vis"          = deriv { phaseOverrides = ghciPostInstall }
  | pname == "github-backup"    = deriv { buildTools = Set.insert "git" buildTools }
  | pname == "gloss-raster"     = deriv { extraLibs = Set.insert "llvm" extraLibs }
  | pname == "GLUT"             = deriv { extraLibs = Set.fromList ["glut","libSM","libICE","libXmu","libXi","mesa"] `Set.union` extraLibs }
  | pname == "gtkglext"         = deriv { pkgConfDeps = Set.insert "pangox_compat" pkgConfDeps }
  | pname == "gtk2hs-buildtools"= deriv { buildDepends = Set.insert "hashtables" buildDepends }
  | pname == "haddock" && version < Version [2,14] []
                                = deriv { buildTools = Set.insert "alex" (Set.insert "happy" buildTools) }
  | pname == "happy"            = deriv { buildTools = Set.insert "perl" buildTools }
  | pname == "haskeline"        = deriv { buildDepends = Set.insert "utf8-string" buildDepends }
  | pname == "haskell-src"      = deriv { buildTools = Set.insert "happy" buildTools }
  | pname == "haskell-src-meta" = deriv { buildDepends = Set.insert "uniplate" buildDepends }
  | pname == "hlibgit2"         = deriv { buildTools = Set.insert "git" buildTools }
  | pname == "HList"            = deriv { buildTools = Set.insert "diffutils" buildTools }
  | pname == "hmatrix"          = deriv { extraLibs = Set.insert "liblapack" (Set.insert "blas" (Set.filter (/= "lapack") extraLibs)) }
  | pname == "hmatrix-special"  = deriv { extraLibs = Set.insert "gsl" extraLibs }
  | pname == "hoogle"           = deriv { testTarget = "--test-option=--no-net" }
  | pname == "idris"            = deriv { buildTools = Set.insert "happy" buildTools, extraLibs = Set.insert "gmp" (Set.insert "boehmgc" extraLibs) }
  | pname == "inline-c-cpp"     = deriv { testDepends = Set.delete "stdc++" testDepends }
  | pname == "language-c-quote" = deriv { buildTools = Set.insert "alex" (Set.insert "happy" buildTools) }
  | pname == "language-java"    = deriv { buildDepends = Set.insert "syb" buildDepends }
  | pname == "lhs2tex"          = deriv { extraLibs = Set.insert "texLive" extraLibs, phaseOverrides = lhs2texPostInstall }
  | pname == "libffi"           = deriv { extraLibs = Set.delete "ffi" extraLibs }
  | pname == "liquid-fixpoint"  = deriv { buildTools = Set.insert "z3" (Set.insert "ocaml" buildTools), configureFlags = Set.insert "-fbuild-external" configureFlags }
  | pname == "liquidhaskell"    = deriv { buildTools = Set.insert "z3" buildTools }
  | pname == "llvm-base"        = deriv { extraLibs = Set.insert "llvm" extraLibs }
  | pname == "llvm-general"     = deriv { doCheck = False }
  | pname == "llvm-general-pure"= deriv { doCheck = False }
  | pname == "MFlow"            = deriv { buildTools = Set.insert "cpphs" buildTools }
  | pname == "multiarg"         = deriv { buildDepends = Set.insert "utf8-string" buildDepends }
  | pname == "ncurses"          = deriv { phaseOverrides = ncursesPatchPhase }
  | pname == "Omega"            = deriv { testDepends = Set.delete "stdc++" testDepends }
  | pname == "OpenAL"           = deriv { extraLibs = Set.insert "openal" extraLibs }
  | pname == "OpenGL"           = deriv { extraLibs = Set.insert "mesa" (Set.insert "libX11" extraLibs) }
  | pname == "pandoc"           = deriv { buildDepends = Set.insert "alex" (Set.insert "happy" buildDepends) }
  | pname == "pcap"             = deriv { extraLibs = Set.insert "libpcap" extraLibs }
  | pname == "persistent"       = deriv { extraLibs = Set.insert "sqlite3" extraLibs }
  | pname == "purescript"       = deriv { buildTools = Set.insert "nodejs" buildTools }
  | pname == "repa-algorithms"  = deriv { extraLibs = Set.insert "llvm" extraLibs }
  | pname == "repa-examples"    = deriv { extraLibs = Set.insert "llvm" extraLibs }
  | pname == "saltine"          = deriv { extraLibs = Set.map (\x -> if x == "sodium" then "libsodium" else x) extraLibs }
  | pname == "SDL-image"        = deriv { extraLibs = Set.insert "SDL_image" extraLibs }
  | pname == "SDL-mixer"        = deriv { extraLibs = Set.insert "SDL_mixer" extraLibs }
  | pname == "SDL-ttf"          = deriv { extraLibs = Set.insert "SDL_ttf" extraLibs }
  | pname == "sloane"           = deriv { phaseOverrides = sloanePostInstall }
  | pname == "structured-haskell-mode" = deriv { buildTools = Set.insert "emacs" buildTools
                                               , phaseOverrides = structuredHaskellModePostInstall
                                               }
  | pname == "target"           = deriv { buildTools = Set.insert "z3" buildTools }
  | pname == "terminfo"         = deriv { extraLibs = Set.insert "ncurses" extraLibs }
  | pname == "threadscope"      = deriv { configureFlags = Set.insert "--ghc-options=-rtsopts" configureFlags }
  | pname == "thyme"            = deriv { buildTools = Set.insert "cpphs" buildTools }
  | pname == "vacuum"           = deriv { extraLibs = Set.insert "ghc-paths" extraLibs }
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
  | pname == "hzk"              = deriv { testDepends = Set.delete "zookeeper_mt" testDepends, buildTools = Set.insert "zookeeper_mt" buildTools }
  | pname == "z3"               = deriv { phaseOverrides = "preBuild = stdenv.lib.optionalString stdenv.isDarwin \"export DYLD_LIBRARY_PATH=${z3}/lib\";" }
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

ghciPostInstall :: String
ghciPostInstall = unlines
  [ "postInstall = ''"
  , "  ensureDir \"$out/share/ghci\""
  , "  ln -s \"$out/share/$pname-$version/ghci\" \"$out/share/ghci/$pname\""
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

-}
