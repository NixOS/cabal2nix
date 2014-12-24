module Cabal2Nix.Name ( toNixName, toNixName', libNixName, buildToolNixName ) where

import Data.Char

-- | Map Cabal names to Nix attribute names.
toNixName :: String -> String
toNixName = id

-- | The old mapping function. This may be useful to generate a compatibility layer.
toNixName' :: String -> String
toNixName' []      = error "toNixName: empty string is not a valid argument"
toNixName' name    = f name
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
libNixName "adns"                               = return "adns"
libNixName "alsa"                               = return "alsaLib"
libNixName "appindicator-0.1"                   = return "appindicator"
libNixName "appindicator3-0.1"                  = return "appindicator"
libNixName "asound"                             = return "alsaLib"
libNixName "awesomium-1.6.5"                    = return "awesomium"
libNixName "bz2"                                = return "bzip2"
libNixName "cairo-pdf"                          = return "cairo"
libNixName "cairo-ps"                           = return "cairo"
libNixName "cairo"                              = return "cairo"
libNixName "cairo-svg"                          = return "cairo"
libNixName "CEGUIBase-0.7.7"                    = return "CEGUIBase"
libNixName "CEGUIOgreRenderer-0.7.7"            = return "CEGUIOgreRenderer"
libNixName "clutter-1.0"                        = return "clutter"
libNixName "crypto"                             = return "openssl"
libNixName "crypt"                              = []  -- provided by glibc
libNixName "c++"                                = []  -- What is that?
libNixName "dl"                                 = []  -- provided by glibc
libNixName "gconf"                              = return "GConf"
libNixName "gconf-2.0"                          = return "GConf"
libNixName "gdk-2.0"                            = return "gtk"
libNixName "gdk-pixbuf-2.0"                     = return "gdk_pixbuf"
libNixName "gdk-x11-2.0"                        = return "gdk_x11"
libNixName "gio-2.0"                            = return "glib"
libNixName "glib-2.0"                           = return "glib"
libNixName "GL"                                 = return "mesa"
libNixName "GLU"                                = ["freeglut","mesa"]
libNixName "glut"                               = ["freeglut","mesa"]
libNixName "gmime-2.4"                          = return "gmime"
libNixName "gnome-keyring-1"                    = return "gnome_keyring"
libNixName "gnome-keyring"                      = return "gnome_keyring"
libNixName "gnome-vfs-2.0"                      = return "gnome_vfs"
libNixName "gnome-vfs-module-2.0"               = return "gnome_vfs_module"
libNixName "gobject-2.0"                        = return "glib"
libNixName "gstreamer-0.10"                     = return "gstreamer"
libNixName "gstreamer-audio-0.10"               = return "gstreamer-audio"
libNixName "gstreamer-base-0.10"                = return "gstreamer-base"
libNixName "gstreamer-controller-0.10"          = return "gstreamer-controller"
libNixName "gstreamer-dataprotocol-0.10"        = return "gstreamer-dataprotocol"
libNixName "gstreamer-net-0.10"                 = return "gstreamer-net"
libNixName "gstreamer-plugins-base-0.10"        = return "gstreamer-plugins-base"
libNixName "gthread-2.0"                        = return "glib"
libNixName "gtk+-2.0"                           = return "gtk"
libNixName "gtk+-3.0"                           = return "gtk3"
libNixName "gtkglext-1.0"                       = return "gtkglext"
libNixName "gtksourceview-2.0"                  = return "gtksourceview"
libNixName "gtksourceview-3.0"                  = return "gtksourceview"
libNixName "gtk-x11-2.0"                        = return "gtk_x11"
libNixName "icudata"                            = return "icu"
libNixName "icui18n"                            = return "icu"
libNixName "icuuc"                              = return "icu"
libNixName "idn"                                = return "libidn"
libNixName "IL"                                 = return "libdevil"
libNixName "iw"                                 = return "wirelesstools"
libNixName "jack"                               = return "jack2"
libNixName "jpeg"                               = return "libjpeg"
libNixName "libglade-2.0"                       = return "libglade"
libNixName "libgsasl"                           = return "gsasl"
libNixName "librsvg-2.0"                        = return "librsvg"
libNixName "libsoup-gnome-2.4"                  = return "libsoup"
libNixName "libusb-1.0"                         = return "libusb"
libNixName "libxml-2.0"                         = return "libxml2"
libNixName "libzip"                             = return "libzip"
libNixName "libzmq"                             = return "zeromq"
libNixName "m"                                  = []  -- in stdenv
libNixName "mono-2.0"                           = return "mono"
libNixName "ncursesw"                           = return "ncurses"
libNixName "notify"                             = return "libnotify"
libNixName "odbc"                               = return "unixODBC"
libNixName "panelw"                             = return "ncurses"
libNixName "pangocairo"                         = return "pango"
libNixName "pcap"                               = return "libpcap"
libNixName "pcre"                               = return "pcre"
libNixName "pfs-1.2"                            = return "pfstools"
libNixName "png"                                = return "libpng"
libNixName "poppler-glib"                       = return "popplerGlib"
libNixName "portaudio-2.0"                      = return "portaudio"
libNixName "pq"                                 = return "postgresql"
libNixName "pthread"                            = []
libNixName "python-3.3"                         = return "python3"
libNixName "ruby1.8"                            = return "ruby"
libNixName "sane-backends"                      = return "saneBackends"
libNixName "SDL2-2.0"                           = return "SDL2"
libNixName "sdl2"                               = return "SDL2"
libNixName "sndfile"                            = return "libsndfile"
libNixName "sqlite3"                            = return "sqlite"
libNixName "ssl"                                = return "openssl"
libNixName "stdc++.dll"                         = [] -- What is that?
libNixName "stdc++"                             = [] -- What is that?
libNixName "systemd-journal"                    = return "systemd"
libNixName "uuid"                               = return "libossp_uuid";
libNixName "vte-2.90"                           = return "vte"
libNixName "webkit-1.0"                         = return "webkit"
libNixName "webkitgtk-3.0"                      = return "webkitgtk"
libNixName "X11"                                = return "libX11"
libNixName "Xext"                               = return "libXext"
libNixName "xft"                                = return "libXft"
libNixName "Xi"                                 = return "libXi"
libNixName "xml2"                               = return "libxml2"
libNixName "Xpm"                                = return "libXpm"
libNixName "Xrandr"                             = return "libXrandr"
libNixName "Xss"                                = return "libXScrnSaver"
libNixName "Xtst"                               = return "libXtst"
libNixName "Xxf86vm"                            = return "libXxf86vm"
libNixName "zmq"                                = return "zeromq"
libNixName "z"                                  = return "zlib"
libNixName x                                    = return x

-- | Map build tool names to Nix attribute names.
buildToolNixName :: String -> [String]
buildToolNixName "cabal"                        = return "cabal-install"
buildToolNixName "gtk2hsC2hs"                   = return "gtk2hs-buildtools"
buildToolNixName "gtk2hsHookGenerator"          = return "gtk2hs-buildtools"
buildToolNixName "gtk2hsTypeGen"                = return "gtk2hs-buildtools"
buildToolNixName x                              = return (toNixName x)
