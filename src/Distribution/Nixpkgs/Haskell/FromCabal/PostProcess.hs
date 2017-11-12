{-# LANGUAGE OverloadedStrings #-}

module Distribution.Nixpkgs.Haskell.FromCabal.PostProcess ( postProcess, pkg ) where

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
    myName = ident # unPackageName (packageName drv)

    buildDeps :: Set Identifier
    buildDeps = Set.delete myName (setOf (dependencies . haskell . folded . localName) drv)

hooks :: [(Dependency, Derivation -> Derivation)]
hooks =
  [ ("Agda < 2.5", set (executableDepends . tool . contains (pkg "emacs")) True . set phaseOverrides agdaPostInstall)
  , ("Agda >= 2.5", set (executableDepends . tool . contains (pkg "emacs")) True . set phaseOverrides agda25PostInstall)
  , ("alex < 3.1.5",  set (testDepends . tool . contains (pkg "perl")) True)
  , ("alex",  set (executableDepends . tool . contains (bind "self.happy")) True)
  , ("alsa-core", over (metaSection . platforms) (Set.filter (\(Platform _ os) -> os == Linux)))                                                                                                      , ("bustle", set (libraryDepends . pkgconfig . contains "system-glib = pkgs.glib") True)
  , ("bindings-GLFW", over (libraryDepends . system) (Set.union (Set.fromList [bind "pkgs.xorg.libXext", bind "pkgs.xorg.libXfixes"])))
  , ("bindings-lxc", over (metaSection . platforms) (Set.filter (\(Platform _ os) -> os == Linux)))                                                                                                      , ("bustle", set (libraryDepends . pkgconfig . contains "system-glib = pkgs.glib") True)
  , ("bustle", set (libraryDepends . pkgconfig . contains "system-glib = pkgs.glib") True)
  , ("cabal-helper", set doCheck False) -- https://github.com/DanielG/cabal-helper/issues/17
  , ("cabal-install", set doCheck False . set phaseOverrides cabalInstallPostInstall)
  , ("Cabal", set doCheck False) -- test suite doesn't work in Nix
  , ("darcs", set phaseOverrides darcsInstallPostInstall . set doCheck False)
  , ("dbus", set doCheck False) -- don't execute tests that try to access the network
  , ("dns", set testTarget "spec")      -- don't execute tests that try to access the network
  , ("eventstore", over (metaSection . platforms) (Set.filter (\(Platform arch _) -> arch == X86_64)))
  , ("freenect < 1.2.1", over configureFlags (Set.union (Set.fromList ["--extra-include-dirs=${pkgs.freenect}/include/libfreenect", "--extra-lib-dirs=${pkgs.freenect}/lib"])))
  , ("gf", set phaseOverrides gfPhaseOverrides . set doCheck False)
  , ("gi-cairo", giCairoPhaseOverrides)                     -- https://github.com/haskell-gi/haskell-gi/issues/36
  , ("gi-gstaudio", giGstLibOverrides "gst-plugins-base")   -- https://github.com/haskell-gi/haskell-gi/issues/36
  , ("gi-gstbase", giGstLibOverrides "gst-plugins-base")    -- https://github.com/haskell-gi/haskell-gi/issues/36
  , ("gi-gst", giGstLibOverrides "gstreamer")               -- https://github.com/haskell-gi/haskell-gi/issues/36
  , ("gi-gstvideo", giGstLibOverrides "gst-plugins-base")   -- https://github.com/haskell-gi/haskell-gi/issues/36
  , ("gi-javascriptcore < 4.0.0.0", webkitgtk24xHook)       -- https://github.com/haskell-gi/haskell-gi/issues/36
  , ("gio", set (libraryDepends . pkgconfig . contains "system-glib = pkgs.glib") True)
  , ("gi-pangocairo", giCairoPhaseOverrides)                     -- https://github.com/haskell-gi/haskell-gi/issues/36
  , ("gi-pango", giCairoPhaseOverrides)                     -- https://github.com/haskell-gi/haskell-gi/issues/36
  , ("git-annex", gitAnnexHook)
  , ("git-annex >= 6.20170925", set doCheck False)      -- new version of git-annex require their test suite to be run inside of a git checkout
  , ("github-backup", set (executableDepends . tool . contains (pkg "git")) True)
  , ("git", set doCheck False)          -- https://github.com/vincenthz/hit/issues/33
  , ("gi-webkit", webkitgtk24xHook)   -- https://github.com/haskell-gi/haskell-gi/issues/36
  , ("GlomeVec", set (libraryDepends . pkgconfig . contains (bind "self.llvmPackages.llvm")) True)
  , ("goatee-gtk", over (metaSection . platforms) (Set.filter (\(Platform _ os) -> os /= OSX)))
  , ("gtk3", gtk3Hook)
  , ("haddock", haddockHook) -- https://github.com/haskell/haddock/issues/511
  , ("hakyll", set (testDepends . tool . contains (pkg "utillinux")) True) -- test suite depends on "rev"
  , ("haskell-src-exts", set doCheck False)
  , ("hspec-core >= 2.4.4", hspecCoreOverrides)
  , ("hfsevents", hfseventsOverrides)
  , ("HFuse", set phaseOverrides hfusePreConfigure)
  , ("hlibgit2 >= 0.18.0.14", set (testDepends . tool . contains (pkg "git")) True)
  , ("hmatrix", set phaseOverrides "preConfigure = \"sed -i hmatrix.cabal -e '/\\/usr\\//D'\";")
  , ("holy-project", set doCheck False)         -- attempts to access the network
  , ("hoogle", set testTarget "--test-option=--no-net")
  , ("hsignal < 0.2.7.4", set phaseOverrides "prePatch = \"rm -v Setup.lhs\";") -- https://github.com/amcphail/hsignal/issues/1
  , ("hslua", over (libraryDepends . each) (replace (pkg "lua") (pkg "lua5_1")))
  , ("http-client-openssl >= 0.2.0.1", set doCheck False) -- attempts to access the network
  , ("http-client", set doCheck False)          -- attempts to access the network
  , ("http-client-tls >= 0.2.2", set doCheck False) -- attempts to access the network
  , ("http-conduit", set doCheck False)         -- attempts to access the network
  , ("imagemagick", set (libraryDepends . pkgconfig . contains (pkg "imagemagick")) True) -- https://github.com/NixOS/cabal2nix/issues/136
  , ("include-file <= 0.1.0.2", set (libraryDepends . haskell . contains (bind "self.random")) True) -- https://github.com/Daniel-Diaz/include-file/issues/1
  , ("js-jquery", set doCheck False)            -- attempts to access the network
  , ("libconfig", over (libraryDepends . system) (replace "config = null" (pkg "libconfig")))
  , ("liquid-fixpoint", set (executableDepends . system . contains (pkg "ocaml")) True . set (testDepends . system . contains (pkg "z3")) True . set (testDepends . system . contains (pkg "nettools")) True . set (testDepends . system . contains (pkg "git")) True . set doCheck False)
  , ("liquidhaskell", set (testDepends . system . contains (pkg "z3")) True)
  , ("lzma-clib", over (metaSection . platforms) (Set.filter (\(Platform _  os) -> os == Windows)) . set (libraryDepends . haskell . contains (bind "self.only-buildable-on-windows")) False)
  , ("MFlow < 4.6", set (libraryDepends . tool . contains (bind "self.cpphs")) True)
  , ("mwc-random", set doCheck False)
  , ("mysql", set (libraryDepends . system . contains (pkg "mysql")) True)
  , ("network-attoparsec", set doCheck False) -- test suite requires network access
  , ("numeric-qq", set doCheck False) -- test suite doesn't finish even after 1+ days
  , ("opencv", opencvOverrides)
  , ("pandoc >= 1.16.0.2", set doCheck False) -- https://github.com/jgm/pandoc/issues/2709 and https://github.com/fpco/stackage/issues/1332
  , ("pandoc-citeproc", set doCheck False) -- https://github.com/jgm/pandoc-citeproc/issues/172
  , ("pandoc", set jailbreak False) -- jailbreak-cabal break the build
  , ("purescript", set doCheck False) -- test suite doesn't cope with Nix build env
  , ("qtah-cpp-qt5", set (libraryDepends . system . contains (bind "pkgs.qt5.qtbase")) True)
  , ("qtah-qt5", set (libraryDepends . tool . contains (bind "pkgs.qt5.qtbase")) True)
  , ("readline", over (libraryDepends . system) (Set.union (pkgs ["readline", "ncurses"])))
  , ("req", set doCheck False)  -- test suite requires network access
  , ("sbv > 7", set (testDepends . system . contains (pkg "z3")) True)
  , ("sdr", over (metaSection . platforms) (Set.filter (\(Platform arch _) -> arch == X86_64))) -- https://github.com/adamwalker/sdr/issues/2
  , ("shake-language-c", set doCheck False) -- https://github.com/samplecount/shake-language-c/issues/26
  , ("ssh", set doCheck False) -- test suite runs forever, probably can't deal with our lack of network access
  , ("stack", set phaseOverrides stackOverrides . set doCheck False)
  , ("stripe-http-streams", set doCheck False . set (metaSection . broken) False)
  , ("target", set (testDepends . system . contains (pkg "z3")) True)
  , ("terminfo", set (libraryDepends . system . contains (pkg "ncurses")) True)
  , ("text", set doCheck False)         -- break infinite recursion
  , ("thyme", set (libraryDepends . tool . contains (bind "self.cpphs")) True) -- required on Darwin
  , ("twilio", set doCheck False)         -- attempts to access the network
  , ("tz", set phaseOverrides "preConfigure = \"export TZDIR=${pkgs.tzdata}/share/zoneinfo\";")
  , ("udev", over (metaSection . platforms) (Set.filter (\(Platform _ os) -> os == Linux)))                                                                                                      , ("bustle", set (libraryDepends . pkgconfig . contains "system-glib = pkgs.glib") True)
  , ("webkitgtk3-javascriptcore", webkitgtk24xHook)   -- https://github.com/haskell-gi/haskell-gi/issues/36
  , ("webkitgtk3", webkitgtk24xHook)   -- https://github.com/haskell-gi/haskell-gi/issues/36
  , ("websockets", set doCheck False)   -- https://github.com/jaspervdj/websockets/issues/104
  , ("Win32", over (metaSection . platforms) (Set.filter (\(Platform _ os) -> os == Windows)))
  , ("Win32-shortcut", over (metaSection . platforms) (Set.filter (\(Platform _ os) -> os == Windows)))
  , ("wxcore", set (libraryDepends . pkgconfig . contains (pkg "wxGTK")) True)
  , ("wxc", wxcHook)
  , ("X11", over (libraryDepends . system) (Set.union (Set.fromList $ map bind ["pkgs.xorg.libXinerama","pkgs.xorg.libXext","pkgs.xorg.libXrender"])))
  , ("xmonad", set phaseOverrides xmonadPostInstall)
  , ("zip-archive < 0.3.1", over (testDepends . tool) (replace (bind "self.zip") (pkg "zip")))
  , ("zip-archive >= 0.3.1", set (testDepends . tool . contains (pkg "zip")) True)  -- https://github.com/jgm/zip-archive/issues/35
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

haddockHook :: Derivation -> Derivation
haddockHook = set doCheck False
            . set phaseOverrides "preCheck = \"unset GHC_PACKAGE_PATH\";"
            . over (dependencies . haskell) (Set.filter (\b -> view localName b /= "haddock-test"))
            . set (metaSection . broken) False

gitAnnexHook :: Derivation -> Derivation
gitAnnexHook = set phaseOverrides gitAnnexOverrides
             . over (executableDepends . system) (Set.union buildInputs)
             . over (metaSection . platforms) (Set.filter (\(Platform _ os) -> os /= OSX))
  where
    gitAnnexOverrides = unlines
      [ "preConfigure = \"export HOME=$TEMPDIR; patchShebangs .\";"
      , "installPhase = \"make PREFIX=$out BUILDER=: install\";"
      , "checkPhase = ''"
      , "  ln -sf dist/build/git-annex/git-annex git-annex"
      , "  ln -sf git-annex git-annex-shell"
      , "  export PATH+=\":$PWD\""
      , "  git-annex test"
      , "'';"
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
  , "  mkdir -p $doc/share/man/man1"
  , "  mv \"$data/\"**\"/man/\"*[0-9] $doc/share/man/man1/"
  , "  rm \"$data/\"**\"/man/\"*"
  , "'';"
  ]

agdaPostInstall :: String
agdaPostInstall = unlines
  [ "postInstall = ''"
  , "  $out/bin/agda -c --no-main $(find $data/share -name Primitive.agda)"
  , "  $out/bin/agda-mode compile"
  , "'';"
  ]

agda25PostInstall :: String
agda25PostInstall = unlines
  [ "postInstall = ''"
  , "  files=(\"$data/share/ghc-\"*\"/\"*\"-ghc-\"*\"/Agda-\"*\"/lib/prim/Agda/\"{Primitive.agda,Builtin\"/\"*.agda})"
  -- Separate loops to avoid internal error
  , "  for f in \"''${files[@]}\" ; do"
  , "    $out/bin/agda $f"
  , "  done"
  , "  for f in \"''${files[@]}\" ; do"
  , "    $out/bin/agda -c --no-main $f"
  , "  done"
  , "  $out/bin/agda-mode compile"
  , "'';"
  ]

stackOverrides :: String
stackOverrides = unlines
  [ "preCheck = \"export HOME=$TMPDIR\";"
  , "postInstall = ''"
  , "  exe=$out/bin/stack"
  , "  mkdir -p $out/share/bash-completion/completions"
  , "  $exe --bash-completion-script $exe >$out/share/bash-completion/completions/stack"
  , "'';"
  ]

-- Replace a binding for <package> to one to pkgs.gst_all_1.<package>
giGstLibOverrides :: String -> Derivation -> Derivation
giGstLibOverrides package
  = over (libraryDepends . pkgconfig) (replace (pkg (ident # package)) (binding # (ident # package, path # ["pkgs","gst_all_1", ident # package])))

giCairoPhaseOverrides :: Derivation -> Derivation
giCairoPhaseOverrides = over phaseOverrides (++txt)
                      . set (libraryDepends . pkgconfig . contains (pkg "cairo")) True
  where
    txt = unlines [ "preCompileBuildDriver = ''"
                  , "  PKG_CONFIG_PATH+=\":${cairo}/lib/pkgconfig\""
                  , "  setupCompileFlags+=\" $(pkg-config --libs cairo-gobject)\""
                  , "'';"
                  ]

hfseventsOverrides :: Derivation -> Derivation
hfseventsOverrides
  = set isLibrary True
  . over (metaSection . platforms) (Set.filter (\(Platform _ os) -> os == OSX))
  . set (libraryDepends . tool . contains (bind "pkgs.darwin.apple_sdk.frameworks.CoreServices")) True
  . set (libraryDepends . system . contains (bind "pkgs.darwin.apple_sdk.frameworks.Cocoa")) True
  . over (libraryDepends . haskell) (Set.union (Set.fromList (map bind ["self.base", "self.cereal", "self.mtl", "self.text", "self.bytestring"])))

webkitgtk24xHook :: Derivation -> Derivation    -- https://github.com/NixOS/cabal2nix/issues/145
webkitgtk24xHook = set (libraryDepends . pkgconfig . contains (pkg "webkitgtk24x-gtk3")) True
                 . over (libraryDepends . pkgconfig) (Set.filter (\b -> view localName b /= "webkitgtk24x-gtk3"))

opencvOverrides :: Derivation -> Derivation
opencvOverrides = set phaseOverrides "hardeningDisable = [ \"bindnow\" ];"
                . over (libraryDepends . pkgconfig) (replace (pkg "opencv") (pkg "opencv3"))
                . set (configureFlags . contains "--with-gcc=${stdenv.cc}/bin/c++") True
                . set (configureFlags . contains "--with-ld=${stdenv.cc}/bin/c++") True

hspecCoreOverrides :: Derivation -> Derivation   -- https://github.com/hspec/hspec/issues/330
hspecCoreOverrides = set phaseOverrides "testTarget = \"--test-option=--skip --test-option='Test.Hspec.Core.Runner.hspecResult runs specs in parallel'\";"
