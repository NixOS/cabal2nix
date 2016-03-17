{-# LANGUAGE OverloadedStrings #-}

module Distribution.Nixpkgs.Haskell.FromCabal.PostProcess ( postProcess ) where

import Control.Lens
import Data.List.Split
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Set.Lens
import Distribution.Nixpkgs.Haskell
import Distribution.Nixpkgs.Meta
import Distribution.Package
import Distribution.System
import Distribution.Text
import Distribution.Version
import Language.Nix

postProcess :: Derivation -> Derivation
postProcess deriv = foldr ($) (fixGtkBuilds deriv) [ f | (Dependency n vr, f) <- hooks, packageName deriv == n, packageVersion deriv `withinRange` vr ]

fixGtkBuilds :: Derivation -> Derivation
fixGtkBuilds drv = drv & dependencies . pkgconfig %~ Set.filter (not . collidesWithHaskellName)
                       & dependencies . system %~ Set.filter (not . collidesWithHaskellName)
                       & dependencies . tool %~ Set.filter (not . collidesWithHaskellName)
  where
    collidesWithHaskellName :: Binding -> Bool
    collidesWithHaskellName b = view localName b `Set.member` buildDeps

    myName :: Identifier
    myName = ident # n where PackageName n = packageName drv

    buildDeps :: Set Identifier
    buildDeps = Set.delete myName (setOf (dependencies . haskell . folded . localName) drv)

hooks :: [(Dependency, Derivation -> Derivation)]
hooks =
  [ ("alex",  set (executableDepends . tool . contains (bind "self.happy")) True)
  , ("alex < 3.1.5",  set (testDepends . tool . contains (pkg "perl")) True)
  , ("Agda", set (executableDepends . tool . contains (pkg "emacs")) True . set phaseOverrides agdaPostInstall)
  , ("bindings-GLFW", over (libraryDepends . system) (Set.union (Set.fromList [bind "pkgs.xorg.libXext", bind "pkgs.xorg.libXfixes"])))
  , ("bustle", set (libraryDepends . pkgconfig . contains (binding # ("system-glib", path # ["pkgs","glib"]))) True)
  , ("cabal-install", set phaseOverrides cabalInstallPostInstall)
  , ("cabal-helper", set doCheck False) -- https://github.com/DanielG/cabal-helper/issues/17
  , ("darcs", set phaseOverrides darcsInstallPostInstall)
  , ("dbus", set doCheck False) -- don't execute tests that try to access the network
  , ("dns", set testTarget "spec")      -- don't execute tests that try to access the network
  , ("eventstore", over (metaSection . platforms) (Set.filter (\(Platform arch _) -> arch == X86_64)))
  , ("freenect < 1.2.1", over configureFlags (Set.union (Set.fromList ["--extra-include-dirs=${pkgs.freenect}/include/libfreenect", "--extra-lib-dirs=${pkgs.freenect}/lib"])))
  , ("gf", set phaseOverrides gfPhaseOverrides . set doCheck False)
  , ("git-annex", gitAnnexHook)
  , ("github-backup", set (executableDepends . tool . contains (pkg "git")) True)
  , ("GlomeVec", set (libraryDepends . pkgconfig . contains (bind "self.llvmPackages.llvm")) True)
  , ("goatee-gtk", over (metaSection . platforms) (Set.filter (\(Platform _ os) -> os /= OtherOS "darwin")))
  , ("gtk3", gtk3Hook)
  , ("haddock", set phaseOverrides "preCheck = \"unset GHC_PACKAGE_PATH\";")
  , ("hfsevents", over (metaSection . platforms) (Set.filter (\(Platform _ os) -> os == OtherOS "darwin")))
  , ("HFuse", set phaseOverrides hfusePreConfigure)
  , ("hlibgit2 == 0.18.0.14", set (testDepends . tool . contains (pkg "git")) True)
  , ("hmatrix", set phaseOverrides "preConfigure = \"sed -i hmatrix.cabal -e 's@/usr/@/dont/hardcode/paths/@'\";")
  , ("holy-project", set doCheck False)         -- attempts to access the network
  , ("hslua", over (libraryDepends . each) (replace (pkg "lua") (pkg "lua5_1")))
  , ("hsignal < 0.2.7.4", set phaseOverrides "prePatch = \"rm -v Setup.lhs\";") -- https://github.com/amcphail/hsignal/issues/1
  , ("http-client", set doCheck False)          -- attempts to access the network
  , ("http-conduit", set doCheck False)         -- attempts to access the network
  , ("imagemagick", set (libraryDepends . pkgconfig . contains (pkg "imagemagick")) True) -- https://github.com/NixOS/cabal2nix/issues/136
  , ("include-file <= 0.1.0.2", set (libraryDepends . haskell . contains (bind "self.random")) True) -- https://github.com/Daniel-Diaz/include-file/issues/1
  , ("jsaddle", set (dependencies . haskell . contains (bind "self.ghcjs-base")) False)
  , ("js-jquery", set doCheck False)            -- attempts to access the network
  , ("libconfig", over (libraryDepends . system) (replace (binding # ("config", path # ["null"])) (pkg "libconfig")))
  , ("liquid-fixpoint", set (executableDepends . system . contains (pkg "ocaml")) True . set (testDepends . system . contains (pkg "z3")) True)
  , ("liquidhaskell", set (testDepends . system . contains (pkg "z3")) True)
  , ("MFlow < 4.6", set (libraryDepends . tool . contains (bind "self.cpphs")) True)
  , ("mysql", set (libraryDepends . system . contains (pkg "mysql")) True)
  , ("mwc-random", set doCheck False)
  , ("pandoc-citeproc", set doCheck False) -- https://github.com/jgm/pandoc-citeproc/issues/172
  , ("readline", over (libraryDepends . system) (Set.union (pkgs ["readline", "ncurses"])))
  , ("rocksdb-haskell", set (metaSection . platforms) (Set.singleton (Platform X86_64 Linux)))
  , ("sdr", over (metaSection . platforms) (Set.filter (\(Platform arch _) -> arch == X86_64))) -- https://github.com/adamwalker/sdr/issues/2
  , ("shake-language-c", set doCheck False) -- https://github.com/samplecount/shake-language-c/issues/26
  , ("stack", set phaseOverrides stackOverrides)
  , ("stripe-http-streams", set doCheck False . set (metaSection . broken) False)
  , ("text", set doCheck False)         -- break infinite recursion
  , ("target", set (testDepends . system . contains (pkg "z3")) True)
  , ("terminfo", set (libraryDepends . system . contains (pkg "ncurses")) True)
  , ("thyme", set (libraryDepends . tool . contains (bind "self.cpphs")) True) -- required on Darwin
  , ("websockets", set doCheck False)   -- https://github.com/jaspervdj/websockets/issues/104
  , ("wxcore", set (libraryDepends . pkgconfig . contains (pkg "wxGTK")) True)
  , ("wxc", wxcHook)
  , ("X11", over (libraryDepends . system) (Set.union (Set.fromList $ map bind ["pkgs.xorg.libXinerama","pkgs.xorg.libXext","pkgs.xorg.libXrender"])))
  , ("xmonad", set phaseOverrides xmonadPostInstall)
  , ("zip-archive", over (testDepends . tool) (replace (bind "self.zip") (pkg "zip")))
  ]

pkg :: Identifier -> Binding
pkg i = binding # (i, path # ["pkgs",i])

pkgs :: [Identifier] -> Set Binding
pkgs = Set.fromList . map pkg

bind :: String -> Binding
bind s = binding # (i, path # is)
  where
    is = map (review ident) (splitOn "." s)
    i = last is

replace :: Binding -> Binding -> Set Binding -> Set Binding
replace old new bs = if old `Set.member` bs then Set.insert new (Set.delete old bs) else bs

-- TODO: I need to figure out how to conveniently replace a binding in a set.
gtk3Hook :: Derivation -> Derivation    -- https://github.com/NixOS/cabal2nix/issues/145
gtk3Hook = set (libraryDepends . pkgconfig . contains (pkg "gtk3")) True
         . over (libraryDepends . pkgconfig) (Set.filter (\b -> view localName b /= "gtk3"))

gitAnnexHook :: Derivation -> Derivation
gitAnnexHook = set phaseOverrides gitAnnexOverrides
             . over (executableDepends . system) (Set.union buildInputs)
             . over (metaSection . platforms) (Set.filter (\(Platform _ os) -> os /= OtherOS "darwin"))
  where
    gitAnnexOverrides = unlines
      [ "preConfigure = \"export HOME=$TEMPDIR; patchShebangs .\";"
      , "postBuild = \"ln -sf dist/build/git-annex/git-annex git-annex\";"
      , "installPhase = \"make PREFIX=$out CABAL=./Setup BUILDER=./Setup install\";"
      , "checkPhase = \"./git-annex test\";"
      , "enableSharedExecutables = false;"
      ]
    buildInputs = pkgs ["git","rsync","gnupg","curl","wget","lsof","openssh","which","bup","perl"]

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
wxcHook drv = drv & libraryDepends . system %~ Set.union (Set.fromList [pkg "mesa", bind "pkgs.xorg.libX11"])
                  & libraryDepends . pkgconfig . contains (pkg "wxGTK") .~ True
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

agdaPostInstall :: String
agdaPostInstall = unlines
  [ "postInstall = ''"
  , "  $out/bin/agda -c --no-main $(find $out/share -name Primitive.agda)"
  , "  $out/bin/agda-mode compile"
  , "'';"
  ]

stackOverrides :: String
stackOverrides = unlines
  [ "enableSharedExecutables = false;"
  , "postInstall = ''"
  , "  exe=$out/bin/stack"
  , "  mkdir -p $out/share/bash-completion/completions"
  , "  $exe --bash-completion-script $exe >$out/share/bash-completion/completions/stack"
  , "'';"
  ]

{-
postProcess' :: Derivation -> Derivation
postProcess' deriv@(MkDerivation {..})
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
  | pname == "ghc-heap-view"    = deriv { phaseOverrides = ghciPostInstall }
  | pname == "ghc-mod"          = deriv { phaseOverrides = ghcModPostInstall pname version, buildTools = Set.insert "emacs" buildTools }
  | pname == "ghc-parser"       = deriv { buildTools = Set.insert "cpphs" (Set.insert "happy" buildTools)
                                        , phaseOverrides = ghcParserPatchPhase }
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
  | pname == "idris"            = deriv { buildTools = Set.insert "happy" buildTools, extraLibs = Set.insert "gmp" (Set.insert "boehmgc" extraLibs) }
  | pname == "inline-c-cpp"     = deriv { testDepends = Set.delete "stdc++" testDepends }
  | pname == "language-c-quote" = deriv { buildTools = Set.insert "alex" (Set.insert "happy" buildTools) }
  | pname == "language-java"    = deriv { buildDepends = Set.insert "syb" buildDepends }
  | pname == "lhs2tex"          = deriv { extraLibs = Set.insert "texLive" extraLibs, phaseOverrides = lhs2texPostInstall }
  | pname == "libffi"           = deriv { extraLibs = Set.delete "ffi" extraLibs }
  | pname == "liquid-fixpoint"  = deriv { buildTools = Set.insert "z3" (Set.insert "ocaml" buildTools), configureFlags = Set.insert "-fbuild-external" configureFlags }
  | pname == "liquidhaskell"    = deriv { buildTools = Set.insert "z3" buildTools }
  | pname == "multiarg"         = deriv { buildDepends = Set.insert "utf8-string" buildDepends }
  | pname == "ncurses"          = deriv { phaseOverrides = ncursesPatchPhase }
  | pname == "Omega"            = deriv { testDepends = Set.delete "stdc++" testDepends }
  | pname == "OpenAL"           = deriv { extraLibs = Set.insert "openal" extraLibs }
  | pname == "OpenGL"           = deriv { extraLibs = Set.insert "mesa" (Set.insert "libX11" extraLibs) }
  | pname == "pandoc"           = deriv { buildDepends = Set.insert "alex" (Set.insert "happy" buildDepends) }
  | pname == "persistent"       = deriv { extraLibs = Set.insert "sqlite3" extraLibs }
  | pname == "purescript"       = deriv { buildTools = Set.insert "nodejs" buildTools }
  | pname == "repa-algorithms"  = deriv { extraLibs = Set.insert "llvm" extraLibs }
  | pname == "repa-examples"    = deriv { extraLibs = Set.insert "llvm" extraLibs }
  | pname == "SDL-image"        = deriv { extraLibs = Set.insert "SDL_image" extraLibs }
  | pname == "SDL-mixer"        = deriv { extraLibs = Set.insert "SDL_mixer" extraLibs }
  | pname == "SDL-ttf"          = deriv { extraLibs = Set.insert "SDL_ttf" extraLibs }
  | pname == "sloane"           = deriv { phaseOverrides = sloanePostInstall }
  | pname == "structured-haskell-mode" = deriv { buildTools = Set.insert "emacs" buildTools
                                               , phaseOverrides = structuredHaskellModePostInstall
                                               }
  | pname == "target"           = deriv { buildTools = Set.insert "z3" buildTools }
  | pname == "threadscope"      = deriv { configureFlags = Set.insert "--ghc-options=-rtsopts" configureFlags }
  | pname == "vacuum"           = deriv { extraLibs = Set.insert "ghc-paths" extraLibs }
  | pname == "wxcore"           = deriv { extraLibs = Set.fromList ["wxGTK","mesa","libX11"] `Set.union` extraLibs }
  | pname == "X11-xft"          = deriv { extraLibs = Set.fromList ["pkgconfig","freetype","fontconfig"] `Set.union` extraLibs
                                        , configureFlags = Set.insert "--extra-include-dirs=${freetype}/include/freetype2" configureFlags
                                        }
-- Unbreak packages during hackage2nix generation:

  | pname == "hnetcdf"          = deriv { testDepends = Set.delete "netcdf" testDepends }
  | pname == "SDL2-ttf"         = deriv { buildDepends = Set.delete "SDL2" buildDepends }
  | pname == "hzk"              = deriv { testDepends = Set.delete "zookeeper_mt" testDepends, buildTools = Set.insert "zookeeper_mt" buildTools }
  | pname == "z3"               = deriv { phaseOverrides = "preBuild = stdenv.lib.optionalString stdenv.isDarwin \"export DYLD_LIBRARY_PATH=${z3}/lib\";" }
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
