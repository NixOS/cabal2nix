module Cabal2Nix.Name ( toNixName, libNixName, buildToolNixName ) where

import Data.Char

-- | Map Cabal names to Nix attribute names.
toNixName :: String -> String
toNixName []      = error "toNixName: empty string is not a valid argument"
toNixName name    = f name
  where
    f []                            = []
    f ('-':c:cs) | c `notElem` "-"  = toUpper c : f cs
    f ('-':_)                       = error ("unexpected package name " ++ show name)
    f (c:cs)                        = c : f cs

-- | Map libraries to Nix packages.
-- TODO: This should probably be configurable. We also need to consider the
-- possibility of name clashes with Haskell libraries. I have included
-- identity mappings to incicate that I have verified their correctness.
libNixName :: String -> [String]
libNixName "adns"               = return "adns"
libNixName "alsa"               = return "alsaLib"
libNixName "cairo-pdf"          = return "cairo"
libNixName "cairo-ps"           = return "cairo"
libNixName "cairo"              = return "cairo"
libNixName "cairo-svg"          = return "cairo"
libNixName "crypto"             = return "openssl"
libNixName "gconf-2.0"          = return "gconf"
libNixName "gio-2.0"            = return "glib"
libNixName "glib-2.0"           = return "glib"
libNixName "GL"                 = return "mesa"
libNixName "GLU"                = ["freeglut","mesa"]
libNixName "glut"               = ["freeglut","mesa"]
libNixName "gnome-keyring-1"    = return "gnome_keyring"
libNixName "gnome-keyring"      = return "gnome_keyring"
libNixName "gobject-2.0"        = return "glib"
libNixName "gstreamer-0.10"     = return "gstreamer"
libNixName "gstreamer-audio-0.10"               = return "gstreamer-audio"
libNixName "gstreamer-base-0.10"                = return "gstreamer-base"
libNixName "gstreamer-controller-0.10"          = return "gstreamer-controller"
libNixName "gstreamer-dataprotocol-0.10"        = return "gstreamer-dataprotocol"
libNixName "gstreamer-net-0.10"                 = return "gstreamer-net"
libNixName "gstreamer-plugins-base-0.10"        = return "gstreamer-plugins-base"
libNixName "gthread-2.0"        = return "glib"
libNixName "gtk+-2.0"           = return "gtk"
libNixName "gtkglext-1.0"       = return "gtkglext"
libNixName "gtksourceview-2.0"  = return "gtksourceview"
libNixName "icudata"            = return "icu"
libNixName "icui18n"            = return "icu"
libNixName "icuuc"              = return "icu"
libNixName "idn"                = return "libidn"
libNixName "IL"                 = return "libdevil"
libNixName "iw"                 = return "wirelesstools"
libNixName "jpeg"               = return "libjpeg"
libNixName "libglade-2.0"       = return "libglade"
libNixName "libgsasl"           = return "gsasl"
libNixName "librsvg-2.0"        = return "librsvg"
libNixName "libusb-1.0"         = return "libusb"
libNixName "libxml-2.0"         = return "libxml2"
libNixName "libzip"             = return "libzip"
libNixName "m"                  = []  -- in stdenv
libNixName "ncursesw"           = return "ncurses"
libNixName "panelw"             = return "ncurses"
libNixName "pangocairo"         = return "pango"
libNixName "pcre"               = return "pcre"
libNixName "png"                = return "libpng"
libNixName "pq"                 = return "postgresql"
libNixName "pthread"            = []
libNixName "sndfile"            = return "libsndfile"
libNixName "sqlite3"            = return "sqlite"
libNixName "ssl"                = return "openssl"
libNixName "stdc++"             = []  -- in stdenv
libNixName "webkit-1.0"         = return "webkit"
libNixName "X11"                = return "libX11"
libNixName "Xext"               = return "libXext"
libNixName "xft"                = return "libXft"
libNixName "xml2"               = return "libxml2"
libNixName "Xrandr"             = return "libXrandr"
libNixName "Xss"                = return "libXScrnSaver"
libNixName "Xtst"               = return "libXtst"
libNixName "zmq"                = return "zeromq"
libNixName "z"                  = return "zlib"
libNixName x                    = return x

-- | Map build tool names to Nix attribute names.
buildToolNixName :: String -> [String]
buildToolNixName "cabal"                = return "cabalInstall"
buildToolNixName "gtk2hsC2hs"           = return "gtk2hsBuildtools"
buildToolNixName "gtk2hsHookGenerator"  = return "gtk2hsBuildtools"
buildToolNixName "gtk2hsTypeGen"        = return "gtk2hsBuildtools"
buildToolNixName x                      = return (toNixName x)
