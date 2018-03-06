{-# LANGUAGE OverloadedStrings #-}

module Distribution.Nixpkgs.Haskell.FromCabal.Name ( toNixName, libNixName, buildToolNixName ) where

import Data.Maybe
import Data.String
import Distribution.Package
import Distribution.Text
import Language.Nix

-- | Map Cabal names to Nix attribute names.
toNixName :: PackageName -> Identifier
toNixName "" = error "toNixName: invalid empty package name"
toNixName n  = fromString (unPackageName n)

-- | Map library names specified in Cabal files to Nix package identifiers.
--
-- TODO: This list should not be hard-coded here; it belongs into the Nixpkgs
--       repository.
--
-- TODO: Re-use hook matching system from PostProcess.hs here.

libNixName :: String -> [Identifier]
libNixName ""                                   = []
libNixName "adns"                               = return "adns"
libNixName "alsa"                               = return "alsaLib"
libNixName "alut"                               = return "freealut"
libNixName "appindicator-0.1"                   = return "libappindicator-gtk2"
libNixName "appindicator3-0.1"                  = return "libappindicator-gtk3"
libNixName "asound"                             = return "alsaLib"
libNixName "b2"                                 = return "libb2"
libNixName "bz2"                                = return "bzip2"
libNixName "c++"                                = []  -- What is that?
libNixName "cairo-gobject"                      = return "cairo"
libNixName "cairo-pdf"                          = return "cairo"
libNixName "cairo-ps"                           = return "cairo"
libNixName "cairo-svg"                          = return "cairo"
libNixName "crypt"                              = []  -- provided by glibc
libNixName "crypto"                             = return "openssl"
libNixName "curses"                             = return "ncurses"
libNixName "dl"                                 = []  -- provided by glibc
libNixName "fftw3"                              = return "fftw"
libNixName "fftw3f"                             = return "fftwFloat"
libNixName "gconf"                              = return "GConf"
libNixName "gconf-2.0"                          = return "GConf"
libNixName "gdk-2.0"                            = return "gtk2"
libNixName "gdk-3.0"                            = return "gtk3"
libNixName "gdk-pixbuf-2.0"                     = return "gdk_pixbuf"
libNixName "gdk-x11-2.0"                        = return "gdk_x11"
libNixName "gio-2.0"                            = return "glib"
libNixName "GL"                                 = return "libGL"
libNixName "GLU"                                = ["libGLU","libGL"]
libNixName "glut"                               = ["freeglut","libGLU","libGL"]
libNixName "gnome-keyring"                      = return "gnome-keyring"
libNixName "gnome-keyring-1"                    = return "gnome-keyring"
libNixName "gnome-vfs-2.0"                      = return "gnome-vfs"
libNixName "gnome-vfs-module-2.0"               = return "gnome-vfs_module"
libNixName "gobject-2.0"                        = return "glib"
libNixName "gobject-introspection-1.0"          = return "gobjectIntrospection"
libNixName "gstreamer-audio-0.10"               = return "gst-plugins-base"
libNixName "gstreamer-audio-1.0"                = return "gst-plugins-base"
libNixName "gstreamer-base-0.10"                = return "gst-plugins-base"
libNixName "gstreamer-base-1.0"                 = return "gst-plugins-base"
libNixName "gstreamer-controller-0.10"          = return "gstreamer"
libNixName "gstreamer-dataprotocol-0.10"        = return "gstreamer"
libNixName "gstreamer-net-0.10"                 = return "gst-plugins-base"
libNixName "gstreamer-plugins-base-0.10"        = return "gst-plugins-base"
libNixName "gstreamer-video-1.0"                = return "gst-plugins-base"
libNixName "gthread-2.0"                        = return "glib"
libNixName "gtk+-2.0"                           = return "gtk2"
libNixName "gtk+-3.0"                           = return "gtk3"
libNixName "gtk-x11-2.0"                        = return "gtk_x11"
libNixName "gtksourceview-3.0"                  = return "gtksourceview3"
libNixName "hidapi-libusb"                      = return "hidapi"
libNixName "icudata"                            = return "icu"
libNixName "icui18n"                            = return "icu"
libNixName "icuuc"                              = return "icu"
libNixName "idn"                                = return "libidn"
libNixName "IL"                                 = return "libdevil"
libNixName "ImageMagick"                        = return "imagemagick"
libNixName "Imlib2"                             = return "imlib2"
libNixName "iw"                                 = return "wirelesstools"
libNixName "jack"                               = return "libjack2"
libNixName "javascriptcoregtk-3.0"              = return "webkitgtk24x-gtk3"     -- These are the old APIs, of which 2.4 is the last provider, so map directly to that.
libNixName "javascriptcoregtk-4.0"              = return "webkitgtk"    -- This is the current API, so let it reference an alias for the latest version.
libNixName "jpeg"                               = return "libjpeg"
libNixName "jvm"                                = return "jdk"
libNixName "lapack"                             = return "liblapack"
libNixName "lber"                               = return "openldap"
libNixName "ldap"                               = return "openldap"
libNixName "libavutil"                          = return "ffmpeg"
libNixName "libgsasl"                           = return "gsasl"
libNixName "libpcre"                            = return "pcre"
libNixName "libR"                               = return "R"
libNixName "libsoup-gnome-2.4"                  = return "libsoup"
libNixName "libsystemd"                         = return "systemd"
libNixName "libxml-2.0"                         = return "libxml2"
libNixName "libzip"                             = return "libzip"
libNixName "libzmq"                             = return "zeromq"
libNixName "m"                                  = []  -- in stdenv
libNixName "magic"                              = return "file"
libNixName "MagickWand"                         = return "imagemagick"
libNixName "mpi"                                = return "openmpi"
libNixName "ncursesw"                           = return "ncurses"
libNixName "netsnmp"                            = return "net_snmp"
libNixName "notify"                             = return "libnotify"
libNixName "odbc"                               = return "unixODBC"
libNixName "openblas"                           = return "openblasCompat"
libNixName "panelw"                             = return "ncurses"
libNixName "pangocairo"                         = return "pango"
libNixName "pcap"                               = return "libpcap"
libNixName "pfs-1.2"                            = return "pfstools"
libNixName "png"                                = return "libpng"
libNixName "poppler-glib"                       = return "poppler"
libNixName "pq"                                 = return "postgresql"
libNixName "pthread"                            = []
libNixName "pulse"                              = return "libpulseaudio"
libNixName "pulse-simple"                       = return "libpulseaudio"
libNixName "python-3.3"                         = return "python33"
libNixName "python-3.4"                         = return "python34"
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
libNixName "sctp"                               = return "lksctp-tools" -- This is linux-specific, we should create a common attribute if we ever add sctp support for other systems.
libNixName "sdl2"                               = return "SDL2"
libNixName "ssh2"                               = return "libssh2"
libNixName "sndfile"                            = return "libsndfile"
libNixName "sodium"                             = return "libsodium"
libNixName "sqlite3"                            = return "sqlite"
libNixName "ssl"                                = return "openssl"
libNixName "statgrab"                           = return "libstatgrab"
libNixName "stdc++"                             = [] -- What is that?
libNixName "stdc++.dll"                         = [] -- What is that?
libNixName "systemd-journal"                    = return "systemd"
libNixName "tag_c"                              = return "taglib"
libNixName "taglib_c"                           = return "taglib"
libNixName "udev"                               = return "systemd";
libNixName "uuid"                               = return "libossp_uuid";
libNixName "wayland-client"                     = return "wayland"
libNixName "wayland-cursor"                     = return "wayland"
libNixName "wayland-egl"                        = return "libGL"
libNixName "wayland-server"                     = return "wayland"
libNixName "webkit2gtk-4.0"                     = return "webkitgtk"    -- This is the current API, so let it reference an alias for the latest version.
libNixName "webkit2gtk-web-extension-4.0"       = return "webkitgtk-web-extension"  -- This package doesn't actually exist in nixpkgs at present.
libNixName "webkitgtk-3.0"                      = return "webkitgtk24x-gtk3"     -- These are the old APIs, of which 2.4 is the last provider, so map directly to that
libNixName "X11"                                = return "libX11"
libNixName "xau"                                = return "libXau"
libNixName "Xcursor"                            = return "libXcursor"
libNixName "xerces-c"                           = return "xercesc"
libNixName "Xext"                               = return "libXext"
libNixName "xft"                                = return "libXft"
libNixName "Xi"                                 = return "libXi"
libNixName "Xinerama"                           = return "libXinerama"
libNixName "xkbcommon"                          = return "libxkbcommon"
libNixName "xml2"                               = return "libxml2"
libNixName "Xpm"                                = return "libXpm"
libNixName "Xrandr"                             = return "libXrandr"
libNixName "Xrender"                            = return "libXrender"
libNixName "Xss"                                = return "libXScrnSaver"
libNixName "Xtst"                               = return "libXtst"
libNixName "Xxf86vm"                            = return "libXxf86vm"
libNixName "yaml-0.1"                           = return "libyaml"
libNixName "z"                                  = return "zlib"
libNixName "zmq"                                = return "zeromq"
libNixName x                                    = return (guessNixIdentifier x)

-- | Map build tool names to Nix attribute names.
buildToolNixName :: String -> [Identifier]
buildToolNixName ""                             = return (error "buildToolNixName: invalid empty dependency name")
buildToolNixName "cabal"                        = return "cabal-install"
buildToolNixName "fltk-config"                  = return "fltk"
buildToolNixName "ghc"                          = []
buildToolNixName "gtk2hsC2hs"                   = return "gtk2hs-buildtools"
buildToolNixName "gtk2hsHookGenerator"          = return "gtk2hs-buildtools"
buildToolNixName "gtk2hsTypeGen"                = return "gtk2hs-buildtools"
buildToolNixName "hsc2hs"                       = []
buildToolNixName "nix-build"                    = return "nix"
buildToolNixName "nix-env"                      = return "nix"
buildToolNixName "nix-hash"                     = return "nix"
buildToolNixName "nix-instantiate"              = return "nix"
buildToolNixName "nix-store"                    = return "nix"
buildToolNixName x                              = return (fromString x)

-- | Helper function to extract the package name from a String that may or may
-- not be formatted like a Cabal package identifier.
--
-- >>> guessNixIdentifier "foo-1.0"
-- Identifier "foo"
-- >>> guessNixIdentifier "foo"
-- Identifier "foo"
-- >>> guessNixIdentifier "foo - 0"
-- Identifier "foo - 0"
-- >>> guessNixIdentifier "1foo-1.0"
-- Identifier "1foo"
-- >>> guessNixIdentifier "-foo-1.0"
-- Identifier "-foo-1.0"

guessNixIdentifier :: String -> Identifier
guessNixIdentifier x = fromString (fromMaybe x maybePackageId)
  where
    maybePackageId = unPackageName . pkgName <$> simpleParse x
