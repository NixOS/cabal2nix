{-# LANGUAGE OverloadedStrings #-}

module Distribution.Nixpkgs.Haskell.FromCabal.Name ( toNixName, libNixName, buildToolNixName ) where

import Data.String
import Distribution.Package
import Language.Nix

-- | Map Cabal names to Nix attribute names.
toNixName :: PackageName -> Identifier
toNixName (PackageName "") = error "toNixName: invalid empty package name"
toNixName (PackageName n)  = fromString n

-- | Map libraries to Nix packages.
--
-- TODO: This list should not be hard-coded here; it belongs into the Nixpkgs
-- repository.

libNixName :: String -> [Identifier]
libNixName ""                                   = []
libNixName "adns"                               = return "adns"
libNixName "alsa"                               = return "alsaLib"
libNixName "alut"                               = return "freealut"
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
libNixName "curses"                             = return "ncurses"
libNixName "c++"                                = []  -- What is that?
libNixName "dl"                                 = []  -- provided by glibc
libNixName "fftw3f"                             = return "fftwFloat"
libNixName "fftw3"                              = return "fftw"
libNixName "gconf-2.0"                          = return "GConf"
libNixName "gconf"                              = return "GConf"
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
libNixName "gstreamer-audio-0.10"               = return "gst_plugins_base"
libNixName "gstreamer-base-0.10"                = return "gst_plugins_base"
libNixName "gstreamer-controller-0.10"          = return "gstreamer"
libNixName "gstreamer-dataprotocol-0.10"        = return "gstreamer"
libNixName "gstreamer-net-0.10"                 = return "gst_plugins_base"
libNixName "gstreamer-plugins-base-0.10"        = return "gst_plugins_base"
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
libNixName "ImageMagick"                        = return "imagemagick"
libNixName "Imlib2"                             = return "imlib2"
libNixName "iw"                                 = return "wirelesstools"
libNixName "jack"                               = return "libjack2"
libNixName "jpeg"                               = return "libjpeg"
libNixName "ldap"                               = return "openldap"
libNixName "libavutil"                          = return "ffmpeg"
libNixName "libglade-2.0"                       = return "libglade"
libNixName "libgsasl"                           = return "gsasl"
libNixName "libR"                               = return "R"
libNixName "librsvg-2.0"                        = return "librsvg"
libNixName "libsoup-gnome-2.4"                  = return "libsoup"
libNixName "libsystemd"                         = return "systemd"
libNixName "libusb-1.0"                         = return "libusb"
libNixName "libxml-2.0"                         = return "libxml2"
libNixName "libzip"                             = return "libzip"
libNixName "libzmq"                             = return "zeromq"
libNixName "MagickWand"                         = return "imagemagick"
libNixName "magic"                              = return "file"
libNixName "m"                                  = []  -- in stdenv
libNixName "mono-2.0"                           = return "mono"
libNixName "mpi"                                = return "openmpi"
libNixName "ncursesw"                           = return "ncurses"
libNixName "netsnmp"                            = return "net_snmp"
libNixName "notify"                             = return "libnotify"
libNixName "odbc"                               = return "unixODBC"
libNixName "panelw"                             = return "ncurses"
libNixName "pangocairo"                         = return "pango"
libNixName "pcap"                               = return "libpcap"
libNixName "pcre"                               = return "pcre"
libNixName "pfs-1.2"                            = return "pfstools"
libNixName "png"                                = return "libpng"
libNixName "poppler-glib"                       = return "poppler"
libNixName "portaudio-2.0"                      = return "portaudio"
libNixName "pq"                                 = return "postgresql"
libNixName "pthread"                            = []
libNixName "pulse-simple"                       = return "libpulseaudio"
libNixName "python-3.3"                         = return "python3"
libNixName "Qt5Core"                            = return "qt5"
libNixName "Qt5Gui"                             = return "qt5"
libNixName "Qt5Qml"                             = return "qt5"
libNixName "Qt5Quick"                           = return "qt5"
libNixName "Qt5Widgets"                         = return "qt5"
libNixName "rt"                                 = [] -- in glibc
libNixName "rtlsdr"                             = return "rtl-sdr"
libNixName "ruby1.8"                            = return "ruby"
libNixName "sane-backends"                      = return "saneBackends"
libNixName "sass"                               = return "libsass"
libNixName "SDL2-2.0"                           = return "SDL2"
libNixName "sdl2"                               = return "SDL2"
libNixName "sndfile"                            = return "libsndfile"
libNixName "sodium"                             = return "libsodium"
libNixName "sqlite3"                            = return "sqlite"
libNixName "ssl"                                = return "openssl"
libNixName "stdc++.dll"                         = [] -- What is that?
libNixName "stdc++"                             = [] -- What is that?
libNixName "systemd-journal"                    = return "systemd"
libNixName "tag_c"                              = return "taglib"
libNixName "taglib_c"                           = return "taglib"
libNixName "udev"                               = return "systemd";
libNixName "uuid"                               = return "libossp_uuid";
libNixName "vte-2.90"                           = return "vte"
libNixName "wayland-client"                     = return "wayland"
libNixName "wayland-cursor"                     = return "wayland"
libNixName "wayland-egl"                        = return "mesa"
libNixName "wayland-server"                     = return "wayland"
libNixName "webkit-1.0"                         = return "webkit"
libNixName "webkitgtk-3.0"                      = return "webkit"
libNixName "webkitgtk"                          = return "webkit"
libNixName "X11"                                = return "libX11"
libNixName "xau"                                = return "libXau"
libNixName "Xcursor"                            = return "libXcursor"
libNixName "xerces-c"                           = return "xercesc"
libNixName "Xext"                               = return "libXext"
libNixName "xft"                                = return "libXft"
libNixName "Xinerama"                           = return "libXinerama"
libNixName "Xi"                                 = return "libXi"
libNixName "xkbcommon"                          = return "libxkbcommon"
libNixName "xml2"                               = return "libxml2"
libNixName "Xpm"                                = return "libXpm"
libNixName "Xrandr"                             = return "libXrandr"
libNixName "Xss"                                = return "libXScrnSaver"
libNixName "Xtst"                               = return "libXtst"
libNixName "Xxf86vm"                            = return "libXxf86vm"
libNixName "yaml-0.1"                           = return "libyaml"
libNixName "zmq"                                = return "zeromq"
libNixName "z"                                  = return "zlib"
libNixName x                                    = return (fromString x)

-- | Map build tool names to Nix attribute names.
buildToolNixName :: String -> [Identifier]
buildToolNixName ""                             = return (error "buildToolNixName: invalid empty dependency name")
buildToolNixName "cabal"                        = return "cabal-install"
buildToolNixName "ghc"                          = []
buildToolNixName "gtk2hsC2hs"                   = return "gtk2hs-buildtools"
buildToolNixName "gtk2hsHookGenerator"          = return "gtk2hs-buildtools"
buildToolNixName "gtk2hsTypeGen"                = return "gtk2hs-buildtools"
buildToolNixName "hsc2hs"                       = []
buildToolNixName "nix-build"                    = return "nix"
buildToolNixName "nix-env"                      = return "nix"
buildToolNixName "nix-instantiate"              = return "nix"
buildToolNixName "nix-store"                    = return "nix"
buildToolNixName x                              = return (fromString x)
