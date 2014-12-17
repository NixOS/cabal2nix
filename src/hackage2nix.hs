-- Run: cabal build -j hackage2nix && dist/build/hackage2nix/hackage2nix >~/.nix-defexpr/pkgs/development/haskell-modules/hackage-packages.nix

module Main ( main ) where

import Cabal2Nix.Generate
-- import Cabal2Nix.Name
import Cabal2Nix.Package
import Cabal2Nix.Hackage ( readHashedHackage, Hackage )
import Control.Monad
import Control.Monad.Par.Combinator
import Control.Monad.Par.IO
import Control.Monad.Trans
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Monoid
import Data.Set ( Set )
import qualified Data.Set as Set
import Distribution.NixOS.Derivation.Cabal
import Distribution.NixOS.PrettyPrinting hiding ( (<>) )
import Distribution.PackageDescription hiding ( buildDepends, extraLibs, buildTools )
import Distribution.Text

main :: IO ()
main = readHashedHackage >>= runParIO . generatePackageSet

nixAttr :: String -> Version -> String
nixAttr name _ = name -- ++ "_" ++ [ if c == '.' then '_' else c | c <- display ver ]

generatePackageSet :: Hackage -> ParIO ()
generatePackageSet db = do
  pkgs <- parMapM generatePackage (Map.toList db)
  liftIO $ putStrLn "/* hackage-packages.nix is an auto-generated file -- DO NOT EDIT! */"
  liftIO $ putStrLn ""
  liftIO $ putStrLn "{ pkgs, stdenv, callPackage }:"
  liftIO $ putStrLn ""
  liftIO $ putStrLn "self: {"
  liftIO $ putStrLn ""
  forM_ pkgs $ \(name, version, drv) -> do
    let selectHackageNames :: Set String -> Set String
        selectHackageNames = Set.intersection (Map.keysSet db)

        selectMissingHackageNames  :: Set String -> Set String
        selectMissingHackageNames = flip Set.difference (Map.keysSet db)

        conflicts :: Set String
        conflicts = Set.difference (selectHackageNames $ Set.fromList (extraLibs drv ++ pkgConfDeps drv)) missing

        conflictOverrides :: Doc
        conflictOverrides | Set.null conflicts = empty
                          | otherwise          = text " inherit (pkgs) " <> hsep (map text (Set.toAscList conflicts)) <> text "; "

        missing :: Set String
        missing = Set.union (Set.intersection missingSystemLibraries (Set.fromList (extraLibs drv ++ pkgConfDeps drv ++ buildTools drv)))
                            (selectMissingHackageNames (Set.fromList (buildDepends drv ++ testDepends drv)))

        missingOverrides :: Doc
        missingOverrides | Set.null missing = empty
                         | otherwise        = fcat [ text (' ':dep++" = null;") | dep <- Set.toAscList missing ] <> space

        overrides :: Doc
        overrides = conflictOverrides $+$ missingOverrides

    let drv' = if Set.null missing then drv else drv { metaSection = (metaSection drv) { broken = True } }
    -- when (name /= toNixName' name) $
    --   liftIO $ print $ nest 2 $ (string (toNixName' name) <+> equals <+> text ("self." ++ show (nixAttr name version))) <> semi
    -- liftIO $ print $ nest 2 $ (string (toNixName' name) <+> equals <+> text ("self." ++ show (nixAttr name version))) <> semi
    -- when (name /= toNixName' name) $
    --   liftIO $ print $ nest 2 $ (string name <+> equals <+> text ("self." ++ show (nixAttr name version))) <> semi
    liftIO $ print $ nest 2 $ hang (string (nixAttr name version) <+> equals <+> text "callPackage") 2 (parens (disp drv')) <+> (braces overrides <> semi)
    liftIO $ putStrLn ""
  liftIO $ putStrLn "}"

generatePackage :: (String, Map Version GenericPackageDescription) -> ParIO (String,Version,Derivation)
generatePackage (name, versions) = do
  let latestVersion = Set.findMax (Map.keysSet versions)
      pkgDescription = (Map.!) versions latestVersion
  srcSpec <- liftIO $ sourceFromHackage Nothing (name ++ "-" ++ display latestVersion)
  let drv' = (cabal2nix pkgDescription) { src = srcSpec }
      Just cabalFileHash = lookup "x-cabal-file-hash" (customFieldsPD (packageDescription pkgDescription))
      drv  = drv' { editedCabalFile = if revision drv' == 0 then "" else cabalFileHash }
  return (name, latestVersion, drv)

-- TODO: We should obtain the list of valid attributes in Nixpkgs at
-- runtime instead of hard-coding a list of invalid attributes here.
--
-- TODO: This list is way overspecified; there are many packages in here
-- that are referred to in the buildDepends or testDepends section, i.e.
-- packages that we expect to be available from Hackage (but that
-- aren't).

missingSystemLibraries :: Set String
missingSystemLibraries = Set.fromList
  [ "accelerate-llvm"
  , "accelerate-llvm-multidev"
  , "accelerate-llvm-native"
  , "accelerate-llvm-ptx"
  , "advapi32"
  , "Advapi32"
  , "aether"
  , "alut"
  , "antlr3c"
  , "appindicator"
  , "appindicator3"
  , "applicative"
  , "apr-1"
  , "apr-util-1"
  , "arbb_dev"
  , "b2"
  , "bfd"
  , "blkid"
  , "bluetooth"
  , "camwire_1394"
  , "canlib"
  , "casadi_control"
  , "casadi_core"
  , "casadi_ipopt_interface"
  , "casadi_snopt_interface"
  , "cblas"
  , "CEGUIBase"
  , "CEGUIOgreRenderer"
  , "cgen-hs"
  , "clntsh"
  , "cmph"
  , "codec2"
  , "comctl32"
  , "comdlg32"
  , "com_err"
  , "crypto"
  , "csfml-audio"
  , "csfml-graphics"
  , "csfml-network"
  , "csfml-system"
  , "csfml-window"
  , "csound64"
  , "cudd"
  , "curses"
  , "cusparse"
  , "cwiid"
  , "d3d9"
  , "d3dx9"
  , "dbxml"
  , "dc1394"
  , "dc1394_control"
  , "debian-mirror"
  , "dns_sd"
  , "doublefann"
  , "dsound"
  , "dttools"
  , "easy-data"
  , "EGL"
  , "eibclient"
  , "eng"
  , "epd"
  , "eskit"
  , "esound"
  , "ev"
  , "event"
  , "f77blas"
  , "fann"
  , "fftw3"
  , "fltk"
  , "fltk_images"
  , "fmodex"
  , "fmodex64"
  , "freenect_sync"
  , "friso"
  , "ftd2xx"
  , "g"
  , "gcc_s"
  , "gcrypt"
  , "gdi32"
  , "gdk_x11"
  , "gecodeint"
  , "gecodekernel"
  , "gecodesearch"
  , "gecodeset"
  , "gecodesupport"
  , "GeoIP"
  , "ghcjs-base"
  , "GLESv2"
  , "gnome_keyring"
  , "gnome-vfs"
  , "gnome_vfs_module"
  , "gnomeVfsModule"
  , "gomp"
  , "GraphicsMagick"
  , "grgen"
  , "groonga"
  , "gstreamer-audio"
  , "gstreamer-base"
  , "gstreamer-controller"
  , "gstreamer-dataprotocol"
  , "gstreamer-net"
  , "gstreamer-plugins-base"
  , "gtkC"
  , "gtksourceview"
  , "gtk_x11"
  , "haste-lib"
  , "hasteLib"
  , "help"
  , "hg3dcegui040"
  , "hg3denet040"
  , "hg3dogre040"
  , "hg3dsdl2040"
  , "hg3dsfml040"
  , "HGamer3DCAudio015"
  , "HGamer3DOIS015"
  , "hsql-oracle"
  , "hsql-sqlite"
  , "HTam"
  , "hyperestraier"
  , "hyperleveldb"
  , "ImageMagick"
  , "Imlib2"
  , "imm32"
  , "instance-control"
  , "integer"
  , "integer-simple"
  , "intel_aes"
  , "Judy"
  , "jvm"
  , "K8055D"
  , "kernel32"
  , "kics"
  , "language-css-attoparsec"
  , "lapack"
  , "lapacke"
  , "lber"
  , "lbfgsb"
  , "ldap"
  , "leksah-dummy"
  , "leksah-main"
  , "leksah-plugin-pane"
  , "LEXER"
  , "libaosd"
  , "libavcodec"
  , "libavformat"
  , "libavutil"
  , "libc"
  , "libdpkg"
  , "libglade"
  , "libkmod"
  , "libnm-glib"
  , "librrd"
  , "libsoup_gnome"
  , "libswscale"
  , "libudev"
  , "libxfconf-0"
  , "libxine"
  , "llvm-config"
  , "MagickCore"
  , "MagickWand"
  , "mozembed"
  , "mpdec"
  , "mpi"
  , "msimg32"
  , "mx"
  , "mysqlclient"
  , "mysqlConfig"
  , "netsnmp"
  , "newrelic-collector-client"
  , "newrelic-common"
  , "newrelic-transaction"
  , "nm-glib"
  , "notify"
  , "nsl"
  , "nvidia_x11"
  , "nvvm"
  , "objc"
  , "odbc"
  , "ogg"
  , "OGRE"
  , "OgreMain"
  , "OgrePaging"
  , "OgreProperty"
  , "OgreRTShaderSystem"
  , "OgreTerrain"
  , "OIS"
  , "ole32"
  , "oleaut32"
  , "opcodes"
  , "opencc"
  , "opencv_calib3d"
  , "opencv_contrib"
  , "opencv_core"
  , "opencv_features2d"
  , "opencv_flann"
  , "opencv_gpu"
  , "opencv_highgui"
  , "opencv_imgproc"
  , "opencv_legacy"
  , "opencv_ml"
  , "opencv_objdetect"
  , "opencv_video"
  , "OpenNI2"
  , "ovr"
  , "papi"
  , "pfs"
  , "pHash"
  , "plplotd-gnome2"
  , "popplerGlib"
  , "qdbm"
  , "Qt5Core"
  , "Qt5Gui"
  , "Qt5Qml"
  , "Qt5Quick"
  , "Qt5Widgets"
  , "QtCore"
  , "QtWebKit"
  , "QuadProgpp"
  , "quickcheck-lio-instances"
  , "rados"
  , "raptor"
  , "raw1394"
  , "rdkafka"
  , "resolv"
  , "riak-bump"
  , "rocksdb"
  , "rt"
  , "scsynth"
  , "SDL2_ttf"
  , "sedna"
  , "sfml-network"
  , "sfml-system"
  , "sfml-window"
  , "shell32"
  , "shfolder"
  , "sipc"
  , "sixense"
  , "sixense_x64"
  , "sqlplus"
  , "ssh2"
  , "stats-web"
  , "string-templates"
  , "swipl"
  , "Synt"
  , "systemd-daemon"
  , "systemGraphviz"
  , "tag_c"
  , "taglib_c"
  , "terralib4c"
  , "theora"
  , "tiff"
  , "translib"
  , "UniqueLogicNP"
  , "user32"
  , "util"
  , "virt"
  , "wayland-client"
  , "wayland-cursor"
  , "wayland-egl"
  , "wayland-server"
  , "winmm"
  , "winspool"
  , "wmflite"
  , "ws2_32"
  , "www-minus"
  , "xau"
  , "Xdamage"
  , "xenctrl"
  , "xerces-c"
  , "Xfixes"
  , "Xinerama"
  , "xmmsclient"
  , "xmmsclient-glib"
  , "xqilla"
  , "zephyr"
  , "zeromq"
  , "zookeeper_mt"
  ]
