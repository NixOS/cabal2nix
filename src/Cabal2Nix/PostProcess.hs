{-# LANGUAGE RecordWildCards #-}

module Cabal2Nix.PostProcess ( postProcess ) where

import Distribution.NixOS.Derivation.Cabal
import Data.List

postProcess :: Derivation -> Derivation
postProcess deriv@(MkDerivation {..})
  | pname == "alex"             = deriv { buildTools = "perl":buildTools }
  | pname == "cairo"            = deriv { extraLibs = "pkgconfig":"libc":"cairo":"zlib":extraLibs }
  | pname == "cuda"             = deriv { phaseOverrides = cudaConfigurePhase, extraLibs = "cudatoolkit":"nvidia_x11":"self.stdenv.gcc":extraLibs }
  | pname == "editline"         = deriv { extraLibs = "libedit":extraLibs }
  | pname == "epic"             = deriv { extraLibs = "gmp":"boehmgc":extraLibs, buildTools = "happy":buildTools }
  | pname == "ghc-mod"          = deriv { phaseOverrides = ghcModPostInstall, buildTools = "emacs":buildTools }
  | pname == "glade"            = deriv { extraLibs = "pkgconfig":"libc":extraLibs, pkgConfDeps = "gtkC":delete "gtk" pkgConfDeps }
  | pname == "glib"             = deriv { extraLibs = "pkgconfig":"libc":extraLibs }
  | pname == "GLUT"             = deriv { extraLibs = "glut":"libSM":"libICE":"libXmu":"libXi":"mesa":extraLibs }
  | pname == "gtk"              = deriv { extraLibs = "pkgconfig":"libc":extraLibs, buildDepends = delete "gio" buildDepends }
  | pname == "gtksourceview2"   = deriv { extraLibs = "pkgconfig":"libc":extraLibs }
  | pname == "haddock"          = deriv { buildTools = "alex":"happy":buildTools }
  | pname == "happy"            = deriv { buildTools = "perl":buildTools }
  | pname == "haskell-src"      = deriv { buildTools = "happy":buildTools }
  | pname == "haskell-src-meta" = deriv { buildDepends = "uniplate":buildDepends }
  | pname == "hmatrix"          = deriv { extraLibs = "gsl":"liblapack":"blas":extraLibs }
  | pname == "idris"            = deriv { buildTools = "happy":buildTools }
  | pname == "language-c-quote" = deriv { buildTools = "alex":"happy":buildTools }
  | pname == "leksah-server"    = deriv { buildDepends = "process-leksah":buildDepends }
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
  | pname == "wxcore"           = deriv { extraLibs = "wxGTK":"mesa":"libX11":extraLibs }
  | pname == "wxc"              = deriv { extraLibs = "wxGTK":"mesa":"libX11":extraLibs, phaseOverrides = wxcPostInstall }
  | pname == "X11" && version >= Version [1,6] []
                                = deriv { extraLibs = "libXinerama":"libXext":"libXrender":extraLibs }
  | pname == "X11"              = deriv { extraLibs = "libXinerama":"libXext":extraLibs }
  | pname == "X11-xft"          = deriv { extraLibs = "pkgconfig":"freetype":"fontconfig":extraLibs
                                        , configureFlags = "--extra-include-dirs=${freetype}/include/freetype2":configureFlags
                                        }
  | otherwise                   = deriv

cudaConfigurePhase :: String
cudaConfigurePhase = unlines
  [ "# Perhaps this should be the default in cabal.nix ..."
  , "#"
  , "# The cudatoolkit provides both 64 and 32-bit versions of the"
  , "# library. GHC's linker fails if the wrong version is found first."
  , "# We solve this by eliminating lib64 from the path on 32-bit"
  , "# platforms and putting lib64 first on 64-bit platforms."
  , ""
  , "libPaths = if self.stdenv.is64bit then \"lib64 lib\" else \"lib\";"
  , ""
  , "configurePhase = ''"
  , "  for i in Setup.hs Setup.lhs; do"
  , "    test -f $i && ghc --make $i"
  , "  done"
  , ""
  , "  for p in $extraBuildInputs $propagatedBuildNativeInputs; do"
  , "    if [ -d \"$p/include\" ]; then"
  , "      extraLibDirs=\"$extraLibDirs --extra-include-dir=$p/include\""
  , "    fi"
  , "    for d in $libPaths; do"
  , "      if [ -d \"$p/$d\" ]; then"
  , "        extraLibDirs=\"$extraLibDirs --extra-lib-dir=$p/$d\""
  , "      fi"
  , "    done"
  , "  done"
  , ""
  , "  ./Setup configure --verbose --prefix=\"$out\" $libraryProfiling $extraLibDirs $configureFlags"
  , "'';"
  ]

ghcModPostInstall :: String
ghcModPostInstall = unlines
                    [ "postInstall = ''"
                    , "    cd $out/share/$pname-$version"
                    , "    make"
                    , "    rm Makefile"
                    , "    cd .."
                    , "    ensureDir \"$out/share/emacs\""
                    , "    mv $pname-$version emacs/site-lisp"
                    , "  '';"
                    ]

wxcPostInstall :: String
wxcPostInstall = unlines
                 [ "postInstall = ''"
                 , "    cp -v dist/build/libwxc.so.${self.version} $out/lib/libwxc.so"
                 , "  '';"
                 ]
