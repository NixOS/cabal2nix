{-# LANGUAGE RecordWildCards #-}

module Cabal2Nix.PostProcess ( postProcess ) where

import Distribution.NixOS.Derivation.Cabal
import Data.List

postProcess :: Derivation -> Derivation
postProcess deriv@(MkDerivation {..})
  | pname == "alex"             = deriv { buildTools = "perl":buildTools }
  | pname == "cabal-install" && version >= Version [0,14] []
                                = deriv { phaseOverrides = cabalInstallPostInstall }
  | pname == "cairo"            = deriv { extraLibs = "pkgconfig":"libc":"cairo":"zlib":extraLibs }
  | pname == "cuda"             = deriv { phaseOverrides = cudaConfigurePhase, extraLibs = "cudatoolkit":"nvidia_x11":"self.stdenv.gcc":extraLibs }
  | pname == "darcs"            = deriv { phaseOverrides = darcsInstallPostInstall }
  | pname == "editline"         = deriv { extraLibs = "libedit":extraLibs }
  | pname == "epic"             = deriv { extraLibs = "gmp":"boehmgc":extraLibs, buildTools = "happy":buildTools }
  | pname == "ghc-heap-view"    = deriv { phaseOverrides = ghciPostInstall }
  | pname == "ghc-mod"          = deriv { phaseOverrides = ghcModPostInstall, buildTools = "emacs":buildTools }
  | pname == "ghc-paths"        = deriv { phaseOverrides = ghcPathsPatches }
  | pname == "ghc-vis"          = deriv { phaseOverrides = ghciPostInstall }
  | pname == "git-annex"        = deriv { phaseOverrides = gitAnnexOverrides, buildTools = "git":"rsync":"gnupg1":"curl":"lsof":"openssh":"which":"bup":buildTools }
  | pname == "glade"            = deriv { extraLibs = "pkgconfig":"libc":extraLibs, pkgConfDeps = "gtkC":delete "gtk" pkgConfDeps }
  | pname == "glib"             = deriv { extraLibs = "pkgconfig":"libc":extraLibs }
  | pname == "GLUT"             = deriv { extraLibs = "glut":"libSM":"libICE":"libXmu":"libXi":"mesa":extraLibs }
  | pname == "gtk"              = deriv { extraLibs = "pkgconfig":"libc":extraLibs, buildDepends = delete "gio" buildDepends }
  | pname == "gtksourceview2"   = deriv { extraLibs = "pkgconfig":"libc":extraLibs }
  | pname == "haddock"          = deriv { buildTools = "alex":"happy":buildTools }
  | pname == "happy"            = deriv { buildTools = "perl":buildTools }
  | pname == "haskeline"        = deriv { buildDepends = "utf8String":buildDepends }
  | pname == "haskell-src"      = deriv { buildTools = "happy":buildTools }
  | pname == "haskell-src-meta" = deriv { buildDepends = "uniplate":buildDepends }
  | pname == "hflags"           = deriv { metaSection = metaSection { license = Unknown (Just "Apache-2.0") } }
  | pname == "HFuse"            = deriv { phaseOverrides = hfusePreConfigure }
  | pname == "highlighting-kate"= highlightingKatePostProcessing deriv
  | pname == "hmatrix"          = deriv { extraLibs = "gsl":"liblapack":"blas":extraLibs }
  | pname == "hspec"            = deriv { doCheck = False }
  | pname == "idris"            = if version < Version [0,9,9] []
                                    then deriv { buildTools = "happy":buildTools, extraLibs = "gmp":extraLibs }
                                    else deriv { buildTools = "happy":buildTools, extraLibs = "gmp":"boehmgc":extraLibs }
  | pname == "language-c-quote" = deriv { buildTools = "alex":"happy":buildTools }
  | pname == "language-java"    = deriv { buildDepends = "syb":buildDepends }
  | pname == "leksah-server"    = deriv { buildDepends = "process-leksah":buildDepends }
  | pname == "libffi"           = deriv { extraLibs = delete "ffi" extraLibs }
  | pname == "llvm-base"        = deriv { extraLibs = "llvm":extraLibs }
  | pname == "multiarg"         = deriv { buildDepends = "utf8String":buildDepends }
  | pname == "OpenAL"           = deriv { extraLibs = "openal":extraLibs }
  | pname == "OpenGL"           = deriv { extraLibs = "mesa":"libX11":extraLibs }
  | pname == "pango"            = deriv { extraLibs = "pkgconfig":"libc":extraLibs }
  | pname == "persistent"       = deriv { extraLibs = "sqlite3":extraLibs }
  | pname == "repa-algorithms"  = deriv { extraLibs = "llvm":extraLibs }
  | pname == "repa-examples"    = deriv { extraLibs = "llvm":extraLibs }
  | pname == "SDL-image"        = deriv { extraLibs = "SDL_image":extraLibs }
  | pname == "SDL-mixer"        = deriv { extraLibs = "SDL_mixer":extraLibs }
  | pname == "SDL-ttf"          = deriv { extraLibs = "SDL_ttf":extraLibs }
  | pname == "svgcairo"         = deriv { extraLibs = "libc":extraLibs }
  | pname == "terminfo"         = deriv { extraLibs = "ncurses":extraLibs }
  | pname == "threadscope"      = deriv { configureFlags = "--ghc-options=-rtsopts":configureFlags }
  | pname == "vacuum"           = deriv { extraLibs = "ghcPaths":extraLibs }
  | pname == "wxc"              = deriv { extraLibs = "wxGTK":"mesa":"libX11":extraLibs, phaseOverrides = wxcPostInstall }
  | pname == "wxcore"           = deriv { extraLibs = "wxGTK":"mesa":"libX11":extraLibs }
  | pname == "X11" && version >= Version [1,6] []
                                = deriv { extraLibs = "libXinerama":"libXext":"libXrender":extraLibs }
  | pname == "X11"              = deriv { extraLibs = "libXinerama":"libXext":extraLibs }
  | pname == "X11-xft"          = deriv { extraLibs = "pkgconfig":"freetype":"fontconfig":extraLibs
                                        , configureFlags = "--extra-include-dirs=${freetype}/include/freetype2":configureFlags
                                        }
  | pname == "xmonad"           = deriv { phaseOverrides = xmonadPostInstall }
  | otherwise                   = deriv

cudaConfigurePhase :: String
cudaConfigurePhase = unlines
  [ "# Perhaps this should be the default in cabal.nix ..."
  , "#"
  , "# The cudatoolkit provides both 64 and 32-bit versions of the"
  , "# library. GHC's linker fails if the wrong version is found first."
  , "# We solve this by eliminating lib64 from the path on 32-bit"
  , "# platforms and putting lib64 first on 64-bit platforms."
  , "libPaths = if self.stdenv.is64bit then \"lib64 lib\" else \"lib\";"
  , "configurePhase = ''"
  , "  for i in Setup.hs Setup.lhs; do"
  , "    test -f $i && ghc --make $i"
  , "  done"
  , "  for p in $extraBuildInputs $propagatedNativeBuildInputs; do"
  , "    if [ -d \"$p/include\" ]; then"
  , "      extraLibDirs=\"$extraLibDirs --extra-include-dir=$p/include\""
  , "    fi"
  , "    for d in $libPaths; do"
  , "      if [ -d \"$p/$d\" ]; then"
  , "        extraLibDirs=\"$extraLibDirs --extra-lib-dir=$p/$d\""
  , "      fi"
  , "    done"
  , "  done"
  , "  ./Setup configure --verbose --prefix=\"$out\" $libraryProfiling $extraLibDirs $configureFlags"
  , "'';"
  ]

ghcModPostInstall :: String
ghcModPostInstall = unlines
  [ "postInstall = ''"
  , "  cd $out/share/$pname-$version"
  , "  make"
  , "  rm Makefile"
  , "  cd .."
  , "  ensureDir \"$out/share/emacs\""
  , "  mv $pname-$version emacs/site-lisp"
  , "'';"
  ]

wxcPostInstall :: String
wxcPostInstall = unlines
  [ "postInstall = ''"
  , "  cp -v dist/build/libwxc.so.${self.version} $out/lib/libwxc.so"
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
  { phaseOverrides = "prePatch = \"sed -i -e 's|regex-pcre-builtin|regex-pcre|' highlighting-kate.cabal\";"
  , buildDepends = "regex-pcre" : filter (/="regex-pcre-builtin") buildDepends
  }

xmonadPostInstall :: String
xmonadPostInstall = unlines
  [ "postInstall = ''"
  , "  mkdir -p $out/share/man/man1"
  , "  mv $out/share/xmonad-*/man/*.1 $out/share/man/man1/"
  , "'';"
  ]

gitAnnexOverrides :: String
gitAnnexOverrides = unlines
  [ "preConfigure = \"patchShebangs .\";"
  , "installPhase = \"make PREFIX=$out CABAL=./Setup docs install\";"
  , "checkPhase = ''"
  , "  export HOME=\"$NIX_BUILD_TOP/tmp\""
  , "  mkdir \"$HOME\""
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
  , "  sed -i -e \"s/LANGUAGE FlexibleContexts/LANGUAGE FlexibleContexts, RankNTypes/\" System/Fuse.hsc"
  , "  sed -i -e \"s/E(Exception/E(catch, Exception, IOException/\" System/Fuse.hsc"
  , "  sed -i -e \"s/IO(catch,/IO(/\" System/Fuse.hsc"
  , "  sed -i -e \"s/IO.catch/ E.catch/\" System/Fuse.hsc"
  , "  sed -i -e \"s/const exitFailure/\\\\\\\\(_ :: IOException) -> exitFailure/\" System/Fuse.hsc"
  , "'';"
  ]

ghcPathsPatches :: String
ghcPathsPatches = "patches = [ ./ghc-paths-nix.patch ];"
