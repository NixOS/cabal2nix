{-# LANGUAGE RecordWildCards #-}

module Cabal2Nix.PostProcess ( postProcess ) where

import Distribution.NixOS.Derivation.Cabal
import Data.List

postProcess :: Derivation -> Derivation
postProcess deriv@(MkDerivation {..})
  | pname == "alex"             = deriv { buildTools = "perl":buildTools }
  | pname == "cairo"            = deriv { extraLibs = "pkgconfig":"glibc":"cairo":"zlib":extraLibs }
  | pname == "editline"         = deriv { extraLibs = "libedit":extraLibs }
  | pname == "epic"             = deriv { extraLibs = "gmp":"boehmgc":extraLibs, buildTools = "happy":buildTools }
  | pname == "glade"            = deriv { extraLibs = "pkgconfig":"glibc":extraLibs, pkgConfDeps = "gtkC":(delete "gtk" pkgConfDeps) }
  | pname == "glib"             = deriv { extraLibs = "pkgconfig":"glibc":extraLibs }
  | pname == "GLUT"             = deriv { extraLibs = "glut":"libSM":"libICE":"libXmu":"libXi":"mesa":extraLibs }
  | pname == "gtk"              = deriv { extraLibs = "pkgconfig":"glibc":extraLibs, buildDepends = delete "gio" buildDepends }
  | pname == "gtksourceview2"   = deriv { extraLibs = "pkgconfig":"glibc":extraLibs }
  | pname == "haskell-src"      = deriv { buildTools = "happy":buildTools }
  | pname == "hmatrix"          = deriv { extraLibs = "gsl":"liblapack":"blas":extraLibs }
  | pname == "idris"            = deriv { buildTools = "happy":buildTools }
  | pname == "OpenAL"           = deriv { extraLibs = "openal":extraLibs }
  | pname == "OpenGL"           = deriv { extraLibs = "mesa":"libX11":extraLibs }
  | pname == "pango"            = deriv { extraLibs = "pkgconfig":"glibc":extraLibs }
  | pname == "persistent"       = deriv { extraLibs = "sqlite3":extraLibs }
  | pname == "repa-examples"    = deriv { extraLibs = "llvm":extraLibs }
  | pname == "SDL-image"        = deriv { extraLibs = "SDL_image":extraLibs }
  | pname == "SDL-mixer"        = deriv { extraLibs = "SDL_mixer":extraLibs }
  | pname == "SDL-ttf"          = deriv { extraLibs = "SDL_ttf":extraLibs }
  | pname == "svgcairo"         = deriv { extraLibs = "glibc":extraLibs }
  | pname == "terminfo"         = deriv { extraLibs = "ncurses":extraLibs }
  | pname == "vacuum"           = deriv { extraLibs = "ghcPaths":extraLibs }
  | pname == "wxcore"           = deriv { extraLibs = "wxGTK":"mesa":"libX11":extraLibs }
  | pname == "X11-xft"          = deriv { extraLibs = "pkgconfig":"freetype":"fontconfig":extraLibs }
  | otherwise                   = deriv
