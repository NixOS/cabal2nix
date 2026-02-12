{-# LANGUAGE OverloadedStrings #-}

module Distribution.Nixpkgs.Haskell.FromCabal.Name ( toNixName, libNixName, buildToolNixName ) where

import Data.Maybe
import Data.String
import Distribution.Package
import Distribution.Text
import Language.Nix

-- | Map Cabal names to Nix identifiers that don't need to be quoted.
--
--   Currently this only supports 'PackageName's that consist of nothing but ASCII
--   characters (as needs to be the case with all Hackage packages).
--   Cabal package names are not changed if they already are a Nix identifier
--   that doesn't need quoting (with some notable exceptions). If they would need
--   quoting, they are prefixed with an underscore.
--
--   >>> toNixName $ mkPackageName "cabal2nix"
--   Identifier "cabal2nix"
--   >>> toNixName $ mkPackageName "4Blocks"
--   Identifier "_4Blocks"
--   >>> toNixName $ mkPackageName "assert"
--   Identifier "_assert"
--
--   Package names that clash with attribute names that have a special meaning
--   to the Nix evaluator are also prefixed (e.g.
--   [@type@ is evaluated eagerly]((https://github.com/NixOS/cabal2nix/issues/163)).
--
--   The mapping is intended to be reversible, but this isn't implemented by
--   @cabal2nix@ yet (and untested). It also should not be considered
--   stable yet, in particular the following may be changed:
--
--   - Future versions of @cabal2nix@ may prefix more 'PackageName's.
--   - The mapping may be extended to support all possible 'PackageName's.
--
--   See also:
--
--   - [Cabal documentation on the package name field](https://cabal.readthedocs.io/en/stable/cabal-package-description-file.html#pkg-field-name)
--   - "Language.Nix.Identifier"
--   - [Nix documentation on identifiers](https://nix.dev/manual/nix/2.30/language/identifiers.html#identifier)
toNixName :: PackageName -> Identifier
toNixName n = fromString $
  case unPackageName n of
    "" -> error "toNixName: BUG: received empty package name"
    '_':_ -> error "toNixName: BUG: PackageName starts with an underscore, but shouldn't"
    name
      -- From the Cabal documentation:
      --
      --   A valid package name comprises an alphanumeric ‘word’; or two or more
      --   such words separated by a hyphen character (-). A word cannot be
      --   comprised only of the digits 0 to 9.
      --
      -- Cabal also latin unicode characters while Hackage enforces that package
      -- names are ASCII.
      --
      -- If the package name comes from Hackage, the set of legal characters
      -- ([a-zA-Z0-9-]) is a subset of those permissible as a Nix identifier
      -- without quoting ([a-zA-Z0-9_'-]). The main difference are the rules
      -- governing what may go where. In the following cases a Hackage package
      -- name is not a simple identifier and 'needsQuoting' returns True:
      --
      -- - if the first “word” of the package name starts with a number, e.g. 4Blocks.
      -- - if the package name is the same one of the 'nixKeywords'.
      --
      -- If we prefix these strings with an underscore, they no longer need quoting.
      -- Because Cabal 'PackageName's may not contain underscores this mapped name
      -- can never clash. (Reversing the mapping is very simple at the moment as
      -- a result.)
      --
      -- We additionally prefix perfectly usable identifiers like type and
      -- recurseForDerivations if they have special meaning to the Nix evaluator
      -- (or Hydra etc.) since it may cause evaluation failures if we expose a
      -- package under haskellPackages instead of whatever value(s) Nix may
      -- expect.
      --
      -- TODO: Add mapping for non-ASCII 'PackageName's, using __ prefix (?)
      | needsQuoting name || name `elem` haveSpecialSemantics -> '_':name
      | otherwise -> name
  where
    -- Special attributes that affect the behavior of the Nix evaluator in some way.
    -- See https://github.com/NixOS/cabal2nix/issues/163.
    -- We can ignore underscore prefixed attrs like __toString, __functor.
    -- Only type is the name of a real package at the moment.
    haveSpecialSemantics = [ "type", "outPath", "recurseForDerivations" ]

-- | Map library names specified in Cabal files to Nix package identifiers.
--
-- TODO: This list should not be hard-coded here; it belongs into the Nixpkgs
--       repository.
--
-- TODO: Re-use hook matching system from PostProcess.hs here.

libNixName :: String -> [Identifier]
libNixName ""                                   = []
libNixName "adns"                               = return "adns"
libNixName "alsa"                               = return "alsa-lib"
libNixName "alut"                               = return "freealut"
libNixName "appindicator-0.1"                   = return "libappindicator-gtk2"
libNixName "appindicator3-0.1"                  = return "libappindicator-gtk3"
libNixName "asound"                             = return "alsa-lib"
libNixName "ayatana-appindicator3-0.1"          = return "libayatana-appindicator"
libNixName "b2"                                 = return "libb2"
libNixName "boost_context"                      = return "boost"
libNixName "bz2"                                = return "bzip2"
libNixName "c++"                                = []  -- What is that?
libNixName "cairo-gobject"                      = return "cairo"
libNixName "cairo-pdf"                          = return "cairo"
libNixName "cairo-ps"                           = return "cairo"
libNixName "cairo-svg"                          = return "cairo"
libNixName "clang"                              = return "libclang"
libNixName "clang-3.8"                          = return "libclang"  -- TODO: guessNixIdentifier before libNixName…
libNixName "crypt"                              = return "libxcrypt" -- starting with NixOS 22.11, glibc's libcrypt will no longer be built
libNixName "crypto"                             = return "openssl"
libNixName "curses"                             = return "ncurses"
libNixName "dbusmenu-glib-0.4"                  = return "libdbusmenu"
libNixName "dbusmenu-gtk3-0.4"                  = ["libdbusmenu-gtk3", "gtk3"]
libNixName "dl"                                 = []  -- provided by glibc
libNixName "ff"                                 = return "libff"
libNixName "fftw3"                              = return "fftw"
libNixName "fftw3f"                             = return "fftwFloat"
libNixName "FLAC"                               = return "flac"
libNixName "freetype2"                          = return "freetype"
libNixName "gconf"                              = return "GConf"
libNixName "gconf-2.0"                          = return "GConf"
libNixName "gdk-2.0"                            = return "gtk2"
libNixName "gdk-3.0"                            = return "gtk3"
libNixName "gdk-pixbuf-2.0"                     = return "gdk-pixbuf"
libNixName "gdk-x11-2.0"                        = return "gdk_x11"
libNixName "geos_c"                             = return "geos"
libNixName "gdk-x11-3.0"                        = return "gtk3"
libNixName "gio-2.0"                            = return "glib"
libNixName "GL"                                 = return "libGL"
libNixName "GLEW"                               = return "glew"
libNixName "GLU"                                = ["libGLU","libGL"]
libNixName "glut"                               = ["freeglut","libGLU","libGL"]
libNixName "gmodule"                            = return "glib"
libNixName "gmodule-2.0"                        = return "glib"
libNixName "gnome-keyring"                      = return "libgnome-keyring"
libNixName "gnome-keyring-1"                    = return "libgnome-keyring"
libNixName "gnome-vfs-2.0"                      = return "gnome-vfs"
libNixName "gnome-vfs-module-2.0"               = return "gnome-vfs_module"
libNixName "gobject-2.0"                        = return "glib"
libNixName "gobject-introspection-1.0"          = return "gobject-introspection"
libNixName "graphene-gobject-1.0"               = return "graphene"
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
libNixName "gtk4-atspi"                         = return "gtk4"
libNixName "gtk4-broadway"                      = return "gtk4"
libNixName "gtk4"                               = return "gtk4"
libNixName "gtk4-unix-print"                    = return "gtk4"
libNixName "gtk4-wayland"                       = return "gtk4"
libNixName "gtk4-x11"                           = return "gtk4"
libNixName "gtksourceview-3.0"                  = return "gtksourceview3"
libNixName "gtksourceview-5"                    = return "gtksourceview5"
libNixName "harfbuzz-gobject"                   = return "harfbuzz"
libNixName "harfbuzz-subset"                    = return "harfbuzz"
libNixName "hidapi-libusb"                      = return "hidapi"
libNixName "icu-i18n"                           = return "icu"
libNixName "icu-io"                             = return "icu"
libNixName "icu-uc"                             = return "icu"
libNixName "icudata"                            = return "icu"
libNixName "icui18n"                            = return "icu"
libNixName "icuio"                              = return "icu"
libNixName "icuuc"                              = return "icu"
libNixName "idn"                                = return "libidn"
libNixName "ImageMagick"                        = return "imagemagick"
libNixName "Imlib2"                             = return "imlib2"
libNixName "iw"                                 = return "wirelesstools"
libNixName "jack"                               = return "libjack2"
libNixName "javascriptcoregtk"                  = return "webkitgtk"
libNixName "javascriptcoregtk-4.1"              = return "webkitgtk_4_1"
libNixName "javascriptcoregtk-6.0"              = return "webkitgtk_6_0"
libNixName "jpeg"                               = return "libjpeg"
libNixName "jvm"                                = return "jdk"
libNixName "lapack"                             = return "liblapack"
libNixName "lber"                               = return "openldap"
libNixName "ldap"                               = return "openldap"
libNixName "libavutil"                          = return "ffmpeg"
libNixName "libbrotlienc"                       = return "brotli"
libNixName "libbrotlidec"                       = return "brotli"
libNixName "libcurl"                            = return "curl"
libNixName "libcrypto"                          = return "openssl"
libNixName "libglog"                            = return "glog"
libNixName "libgsasl"                           = return "gsasl"
libNixName "liblzma"                            = return "xz"
libNixName "libnm"                              = return "networkmanager"
libNixName "libpcre"                            = return "pcre"
libNixName "libpcre2"                           = return "pcre2"
libNixName "libpcre2-8"                         = libNixName "libpcre2"
libNixName "libpipewire-0.3"                    = return "pipewire"
libNixName "libqrencode"                        = return "qrencode"
libNixName "libR"                               = return "R"
libNixName "libsecp256k1"                       = return "secp256k1"
libNixName "libsoup-2.4"                        = return "libsoup_2_4"
libNixName "libsoup-gnome-2.4"                  = return "libsoup_2_4"
libNixName "libsoup-3.0"                        = return "libsoup_3"
libNixName "libspa-0.2"                         = return "pipewire"
libNixName "libsystemd"                         = return "systemd"
libNixName "libudev"                            = return "systemd"
libNixName "libxml-2.0"                         = return "libxml2"
libNixName "libxxhash"                          = return "xxHash"
libNixName "libzip"                             = return "libzip"
libNixName "libzmq"                             = return "zeromq"
libNixName "liquid"                             = return "liquid-dsp"
libNixName "llama"                              = return "llama-cpp"
libNixName "lzma"                               = return "xz"
libNixName "m"                                  = []  -- in stdenv
libNixName "magic"                              = return "file"
libNixName "MagickWand"                         = return "imagemagick"
libNixName "mnl"                                = return "libmnl"
libNixName "mpi"                                = return "openmpi"
libNixName "mysql"                              = return "mariadb"
libNixName "ncursesw"                           = return "ncurses"
libNixName "netsnmp"                            = return "net-snmp"
libNixName "nix-cmd"                            = return "nix"
libNixName "nix-expr-c"                         = return "nix"
libNixName "nix-expr"                           = return "nix"
libNixName "nix-fetchers-c"                     = return "nix"
libNixName "nix-fetchers"                       = return "nix"
libNixName "nix-flake-c"                        = return "nix"
libNixName "nix-flake"                          = return "nix"
libNixName "nix-main-c"                         = return "nix"
libNixName "nix-main"                           = return "nix"
libNixName "nix-store-c"                        = return "nix"
libNixName "nix-store"                          = return "nix"
libNixName "nix-util-c"                         = return "nix"
libNixName "nix-util"                           = return "nix"
libNixName "notify"                             = return "libnotify"
libNixName "odbc"                               = return "unixODBC"
libNixName "openblas"                           = return "openblasCompat"
libNixName "panelw"                             = return "ncurses"
libNixName "pangocairo"                         = return "pango"
libNixName "pcap"                               = return "libpcap"
libNixName "pfs-1.2"                            = return "pfstools"
libNixName "png"                                = return "libpng"
libNixName "poppler-cpp"                        = return "poppler"
libNixName "poppler-glib"                       = return "poppler_gi"
libNixName "pq"                                 = return "libpq"
libNixName "pthread"                            = []
libNixName "pulse"                              = return "libpulseaudio"
libNixName "pulse-simple"                       = return "libpulseaudio"
libNixName "python-3.3"                         = return "python33"
libNixName "python-3.4"                         = return "python34"
libNixName "python3-embed"                      = return "python3"
libNixName "Qt5Core"                            = return "qt5"
libNixName "Qt5Gui"                             = return "qt5"
libNixName "Qt5Qml"                             = return "qt5"
libNixName "Qt5Quick"                           = return "qt5"
libNixName "Qt5Widgets"                         = return "qt5"
libNixName "quadprog"                           = return "quadprogpp"
libNixName "rt"                                 = [] -- in glibc
libNixName "rtlsdr"                             = return "rtl-sdr"
libNixName "ruby1.8"                            = return "ruby"
libNixName "sass"                               = return "libsass"
libNixName "sctp"                               = return "lksctp-tools" -- This is linux-specific, we should create a common attribute if we ever add sctp support for other systems.
libNixName "sdl2"                               = return "SDL2"
libNixName "sndfile"                            = return "libsndfile"
libNixName "SoapySDR"                           = return "soapysdr"
libNixName "sodium"                             = return "libsodium"
libNixName "sqlite3"                            = return "sqlite"
libNixName "ssh2"                               = return "libssh2"
libNixName "ssl"                                = return "openssl"
libNixName "statgrab"                           = return "libstatgrab"
libNixName "stdc++"                             = [] -- What is that?
libNixName "stdc++.dll"                         = [] -- What is that?
libNixName "systemd-journal"                    = return "systemd"
libNixName "tag_c"                              = return "taglib"
libNixName "taglib_c"                           = return "taglib"
libNixName "tdjson"                             = return "tdlib"
libNixName "tensorflow"                         = return "libtensorflow"
libNixName "udev"                               = return "systemd";
libNixName "uuid"                               = return "libossp_uuid";
libNixName "vte-2.91"                           = return "vte"
libNixName "vulkan"                             = return "vulkan-loader"
libNixName "wayland-client"                     = return "wayland"
libNixName "wayland-cursor"                     = return "wayland"
libNixName "wayland-egl"                        = return "libGL"
libNixName "wayland-server"                     = return "wayland"
libNixName "webkit"                             = return "webkitgtk" -- this is an alias, so it only works for downstream expressions not in hackage-packages.nix
libNixName "webkit2gtk"                         = return "webkitgtk"
libNixName "webkit2gtk-4.1"                     = return "webkitgtk_4_1"
libNixName "webkit2gtk-6.0"                     = return "webkitgtk_6_0"
libNixName "webkit2gtk-web-extension-4.1"       = return "webkitgtk_4_1"
libNixName "webkit2gtk-web-extension-6.0"       = return "webkitgtk_6_0"
libNixName "wxGTK"                              = return "wxGTK32"
libNixName "X11"                                = return "libx11"
libNixName "x11"                                = return "libx11"
libNixName "xau"                                = return "libxau"
libNixName "Xcursor"                            = return "libxcursor"
libNixName "xerces-c"                           = return "xercesc"
libNixName "Xext"                               = return "libxext"
libNixName "xft"                                = return "libxft"
libNixName "Xi"                                 = return "libxi"
libNixName "Xinerama"                           = return "libxinerama"
libNixName "xkbcommon"                          = return "libxkbcommon"
libNixName "xml2"                               = return "libxml2"
libNixName "Xpm"                                = return "libxpm"
libNixName "Xrandr"                             = return "libxrandr"
libNixName "Xrender"                            = return "libxrender"
libNixName "xscrnsaver"                         = return "libxscrnsaver"
libNixName "Xss"                                = return "libxscrnsaver"
libNixName "Xtst"                               = return "libxtst"
libNixName "Xxf86vm"                            = return "libxxf86vm"
libNixName "yaml"                               = return "libyaml"
libNixName "yaml-0.1"                           = return "libyaml"
libNixName "z"                                  = return "zlib"
libNixName "zmq"                                = return "zeromq"
libNixName "TKBO"                               = return "opencascade-occt"
libNixName "TKBRep"                             = return "opencascade-occt"
libNixName "TKBin"                              = return "opencascade-occt"
libNixName "TKBinL"                             = return "opencascade-occt"
libNixName "TKBinTObj"                          = return "opencascade-occt"
libNixName "TKBinXCAF"                          = return "opencascade-occt"
libNixName "TKBool"                             = return "opencascade-occt"
libNixName "TKCAF"                              = return "opencascade-occt"
libNixName "TKCDF"                              = return "opencascade-occt"
libNixName "TKDCAF"                             = return "opencascade-occt"
libNixName "TKDE"                               = return "opencascade-occt"
libNixName "TKDECascade"                        = return "opencascade-occt"
libNixName "TKDEGLTF"                           = return "opencascade-occt"
libNixName "TKDEIGES"                           = return "opencascade-occt"
libNixName "TKDEOBJ"                            = return "opencascade-occt"
libNixName "TKDEPLY"                            = return "opencascade-occt"
libNixName "TKDESTEP"                           = return "opencascade-occt"
libNixName "TKDESTL"                            = return "opencascade-occt"
libNixName "TKDEVRML"                           = return "opencascade-occt"
libNixName "TKDraw"                             = return "opencascade-occt"
libNixName "TKExpress"                          = return "opencascade-occt"
libNixName "TKFeat"                             = return "opencascade-occt"
libNixName "TKFillet"                           = return "opencascade-occt"
libNixName "TKG2d"                              = return "opencascade-occt"
libNixName "TKG3d"                              = return "opencascade-occt"
libNixName "TKGeomAlgo"                         = return "opencascade-occt"
libNixName "TKGeomBase"                         = return "opencascade-occt"
libNixName "TKHLR"                              = return "opencascade-occt"
libNixName "TKLCAF"                             = return "opencascade-occt"
libNixName "TKMath"                             = return "opencascade-occt"
libNixName "TKMesh"                             = return "opencascade-occt"
libNixName "TKMeshVS"                           = return "opencascade-occt"
libNixName "TKOffset"                           = return "opencascade-occt"
libNixName "TKOpenGl"                           = return "opencascade-occt"
libNixName "TKOpenGlTest"                       = return "opencascade-occt"
libNixName "TKPrim"                             = return "opencascade-occt"
libNixName "TKQADraw"                           = return "opencascade-occt"
libNixName "TKRWMesh"                           = return "opencascade-occt"
libNixName "TKService"                          = return "opencascade-occt"
libNixName "TKShHealing"                        = return "opencascade-occt"
libNixName "TKStd"                              = return "opencascade-occt"
libNixName "TKStdL"                             = return "opencascade-occt"
libNixName "TKTObj"                             = return "opencascade-occt"
libNixName "TKTObjDRAW"                         = return "opencascade-occt"
libNixName "TKTopAlgo"                          = return "opencascade-occt"
libNixName "TKTopTest"                          = return "opencascade-occt"
libNixName "TKV3d"                              = return "opencascade-occt"
libNixName "TKVCAF"                             = return "opencascade-occt"
libNixName "TKViewerTest"                       = return "opencascade-occt"
libNixName "TKXCAF"                             = return "opencascade-occt"
libNixName "TKXDEDRAW"                          = return "opencascade-occt"
libNixName "TKXMesh"                            = return "opencascade-occt"
libNixName "TKXSBase"                           = return "opencascade-occt"
libNixName "TKXSDRAW"                           = return "opencascade-occt"
libNixName "TKXSDRAWDE"                         = return "opencascade-occt"
libNixName "TKXSDRAWGLTF"                       = return "opencascade-occt"
libNixName "TKXSDRAWIGES"                       = return "opencascade-occt"
libNixName "TKXSDRAWOBJ"                        = return "opencascade-occt"
libNixName "TKXSDRAWPLY"                        = return "opencascade-occt"
libNixName "TKXSDRAWSTEP"                       = return "opencascade-occt"
libNixName "TKXSDRAWSTL"                        = return "opencascade-occt"
libNixName "TKXSDRAWVRML"                       = return "opencascade-occt"
libNixName "TKXml"                              = return "opencascade-occt"
libNixName "TKXmlL"                             = return "opencascade-occt"
libNixName "TKXmlTObj"                          = return "opencascade-occt"
libNixName "TKXmlXCAF"                          = return "opencascade-occt"
libNixName "TKernel"                            = return "opencascade-occt"
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
buildToolNixName "utillinux"                    = return "util-linux"
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
