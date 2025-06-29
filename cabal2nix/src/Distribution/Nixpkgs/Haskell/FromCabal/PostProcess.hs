{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Nixpkgs.Haskell.FromCabal.PostProcess ( postProcess, pkg ) where

import Control.Lens
import Control.Monad.Trans.State
import Data.List.Split
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Map.Lens
import Data.Set ( Set )
import qualified Data.Set as Set
import Distribution.Nixpkgs.Haskell
import Distribution.Nixpkgs.Meta
import Distribution.Nixpkgs.License
import Distribution.Package
import Distribution.Types.PackageVersionConstraint
import Distribution.Text
import Distribution.Version
import Language.Nix

postProcess :: Derivation -> Derivation
postProcess deriv =
 foldr (.) id [ f | (PackageVersionConstraint n vr, f) <- hooks, packageName deriv == n, packageVersion deriv `withinRange` vr ]
 . fixGtkBuilds
 . fixBuildDependsForTools
 $ deriv

fixGtkBuilds :: Derivation -> Derivation
fixGtkBuilds drv = drv & dependencies . pkgconfig %~ Set.filter (not . collidesWithHaskellName)
                       & dependencies . system %~ Set.filter (not . collidesWithHaskellName)
                       & dependencies . tool %~ Set.filter (not . collidesWithHaskellName)
  where
    collidesWithHaskellName :: Binding -> Bool
    collidesWithHaskellName b = case buildDeps Map.!? view localName b of
      Nothing -> False -- totally uncollided
      Just p  -> p /= view reference b -- identical is not collision, and important to preserve for cross

    myName :: Identifier
    myName = ident # unPackageName (packageName drv)

    buildDeps :: Map Identifier Path
    buildDeps = Map.delete myName (toMapOf (dependencies . haskell . to Set.toList . traverse . binding . ifolded) drv)

-- Per https://github.com/haskell/cabal/issues/5412 hvr considers
-- `build-depends` providing executables an accident, and fragile one at that,
-- unworthy of any compatibility hacks. But while he and the other Hackage
-- maintainers is dedicated to fixing executables and libraries on Hackage, test
-- suites and benchmarks are not a priority, as it is trivial to skip building
-- test-suites with cabal-install. Nix however wishes to build test suites much
-- more widely, so skipping those components is not an option.
--
-- Between that, and Stack not changing behavior as of
-- https://github.com/commercialhaskell/stack/pull/4132, it seems likely that
-- for a while packages scraped from Hackage will continue to improperly use
-- `build-depends: package-for-tool` instead of `build-tool-depends` (which does
-- also work for Stack). Until that changes, we provide do this to work around
-- those package's brokenness.
fixBuildDependsForTools :: Derivation -> Derivation
fixBuildDependsForTools = foldr (.) id
  [ fmap snd $ runState $ do
      needs <- use $ cloneLens c . haskell . contains p
      cloneLens c . tool . contains p ||= needs
  | (c :: ALens' Derivation BuildInfo) <- [ testDepends, benchmarkDepends ]
  , p <- self <$> [ "hspec-discover"
                  , "tasty-discover"
                  , "hsx2hs"
                  , "markdown-unlit"
                  ]
  ]

hooks :: [(PackageVersionConstraint, Derivation -> Derivation)]
hooks =
  [ ("Agda < 2.5", set (executableDepends . tool . contains (pkg "emacs")) True . set phaseOverrides agdaPostInstall)
  , ("Agda >= 2.5 && < 2.6", set (executableDepends . tool . contains (pkg "emacs")) True . set phaseOverrides agda25PostInstall)
  , ("Agda >= 2.6", set (executableDepends . tool . contains (pkg "emacs")) True)
  , ("alex < 3.1.5",  set (testDepends . tool . contains (pkg "perl")) True)
  , ("alex",  set (executableDepends . tool . contains (self "happy")) True)
  , ("alsa-core", set (metaSection . platforms) (Just $ Set.singleton (NixpkgsPlatformGroup (ident # "linux"))))
  , ("bindings-GLFW", over (libraryDepends . system) (Set.union (Set.fromList [bind "pkgs.xorg.libXext", bind "pkgs.xorg.libXfixes"])))
  , ("bindings-lxc", set (metaSection . platforms) (Just $ Set.singleton (NixpkgsPlatformGroup (ident # "linux"))))
  , ("bustle", bustleOverrides)
  , ("Cabal", set doCheck False) -- test suite doesn't work in Nix
  , ("Cabal >2.2", over (setupDepends . haskell) (Set.union (Set.fromList [self "mtl", self "parsec"]))) -- https://github.com/haskell/cabal/issues/5391
  , ("cabal-helper", set doCheck False) -- https://github.com/DanielG/cabal-helper/issues/17
  , ("cabal-install", set doCheck False . set phaseOverrides cabalInstallPostInstall)
  , ("cabal2nix > 2", cabal2nixOverrides)
  , ("darcs", set phaseOverrides darcsInstallPostInstall . set doCheck False)
  , ("dbus", set doCheck False) -- don't execute tests that try to access the network
  , ("dhall", set doCheck False) -- attempts to access the network
  , ("dns", set testTargets ["spec"])      -- don't execute tests that try to access the network
  , ("eventstore", set (metaSection . platforms) (Just $ Set.singleton (NixpkgsPlatformGroup (ident # "x86_64"))))
  , ("freenect < 1.2.1", over configureFlags (Set.union (Set.fromList ["--extra-include-dirs=${lib.getDev pkgs.freenect}/include/libfreenect", "--extra-lib-dirs=${lib.getLib pkgs.freenect}/lib"])))
  , ("fltkhs", set (libraryDepends . system . contains (pkg "fltk14")) True . over (libraryDepends . pkgconfig) (Set.union (pkgs ["libGLU", "libGL"]))) -- TODO: fltk14 belongs into the *setup* dependencies.
  , ("gf", set phaseOverrides gfPhaseOverrides . set doCheck False)
  , ("gi-cairo", giCairoPhaseOverrides)                     -- https://github.com/haskell-gi/haskell-gi/issues/36
  , ("gi-gdk", set runHaddock True )
  , ("gi-gio", set runHaddock True )
  , ("gi-glib", set runHaddock True )
  , ("gi-gst", giGstLibOverrides "gstreamer")               -- https://github.com/haskell-gi/haskell-gi/issues/36
  , ("gi-gstaudio", giGstLibOverrides "gst-plugins-base")   -- https://github.com/haskell-gi/haskell-gi/issues/36
  , ("gi-gstbase", giGstLibOverrides "gst-plugins-base")    -- https://github.com/haskell-gi/haskell-gi/issues/36
  , ("gi-gstvideo", giGstLibOverrides "gst-plugins-base")   -- https://github.com/haskell-gi/haskell-gi/issues/36
  , ("gi-gtk", set runHaddock True )
  , ("gi-pango", giCairoPhaseOverrides)                     -- https://github.com/haskell-gi/haskell-gi/issues/36
  , ("gi-pango", set runHaddock True )
  , ("gi-pangocairo", giCairoPhaseOverrides)                     -- https://github.com/haskell-gi/haskell-gi/issues/36
  , ("gi-vte", set runHaddock True )
  , ("gio", set (libraryDepends . pkgconfig . contains "system-glib = pkgs.glib") True)
  , ("git", set doCheck False)          -- https://github.com/vincenthz/hit/issues/33
  , ("git-annex >= 6.20170925 && < 6.20171214", set doCheck False)      -- some versions of git-annex require their test suite to be run inside of a git checkout
  , ("github-backup", set (executableDepends . tool . contains (pkg "git")) True)
  , ("GLFW", over (libraryDepends . system) (Set.union (Set.fromList [bind "pkgs.xorg.libXext", bind "pkgs.xorg.libXfixes"])))
  , ("GlomeVec", set (libraryDepends . pkgconfig . contains (bind "self.llvmPackages.llvm")) True)
  , ("graphviz", set (testDepends . system . contains (pkg "graphviz")) True)
  , ("gtk3", gtk3Hook)
  , ("gtkglext", gtkglextHook)
  , ("hakyll", set (testDepends . tool . contains (pkg "util-linux")) True) -- test suite depends on "rev"
  , ("haskell-src-exts", set doCheck False)
  , ("hfsevents", hfseventsOverrides)
  , ("HFuse", set phaseOverrides hfusePreConfigure)
  , ("hlibgit2 >= 0.18.0.14", set (testDepends . tool . contains (pkg "git")) True)
  , ("hmatrix < 0.18.1.1", set phaseOverrides "preConfigure = \"sed -i hmatrix.cabal -e '/\\\\/usr\\\\//D'\";")
  , ("holy-project", set doCheck False)         -- attempts to access the network
  , ("hoogle", set testFlags ["--no-net"])
  , ("hsignal < 0.2.7.4", set phaseOverrides "prePatch = \"rm -v Setup.lhs\";") -- https://github.com/amcphail/hsignal/issues/1
  , ("hslua < 0.9.3", over (libraryDepends . system) (replace (pkg "lua") (pkg "lua5_1")))
  , ("hslua >= 0.9.3 && < 2.0.0", over (libraryDepends . system) (replace (pkg "lua") (pkg "lua5_3")))
  , ("hspec-core >= 2.4.4", hspecCoreOverrides)
  , ("http-client", set doCheck False)          -- attempts to access the network
  , ("http-client-openssl >= 0.2.0.1", set doCheck False) -- attempts to access the network
  , ("http-client-tls >= 0.2.2", set doCheck False) -- attempts to access the network
  , ("http-conduit", set doCheck False)         -- attempts to access the network
  , ("imagemagick", set (libraryDepends . pkgconfig . contains (pkg "imagemagick")) True) -- https://github.com/NixOS/cabal2nix/issues/136
  , ("include-file <= 0.1.0.2", set (libraryDepends . haskell . contains (self "random")) True) -- https://github.com/Daniel-Diaz/include-file/issues/1
  , ("js-jquery", set doCheck False)            -- attempts to access the network
  , ("libconfig", over (libraryDepends . system) (replace "config = null" (pkg "libconfig")))
  , ("libxml", set (configureFlags . contains "--extra-include-dir=${lib.getDev libxml2}/include/libxml2") True)
  , ("liquidhaskell", set (testDepends . system . contains (pkg "z3")) True)
  , ("lua >= 2.0.0 && < 2.2.0", over (libraryDepends . system) (replace (pkg "lua") (pkg "lua5_3")))
  , ("lua >= 2.2.0", over (libraryDepends . system) (replace (pkg "lua") (pkg "lua5_4")))
  , ("lzma-clib", set (metaSection . platforms) (Just $ Set.singleton (NixpkgsPlatformGroup (ident # "windows"))) . set (libraryDepends . haskell . contains (self "only-buildable-on-windows")) False)
  , ("MFlow < 4.6", set (libraryDepends . tool . contains (self "cpphs")) True)
  , ("mwc-random", set doCheck False)
  , ("mysql", set (libraryDepends . system . contains (pkg "libmysqlclient")) True)
  , ("network-attoparsec", set doCheck False) -- test suite requires network access
  , ("numeric-qq", set doCheck False) -- test suite doesn't finish even after 1+ days
  , ("opencv", opencvOverrides)
  , ("pandoc >= 1.16.0.2 && < 2.5", set doCheck False) -- https://github.com/jgm/pandoc/issues/2709 and https://github.com/fpco/stackage/issues/1332
  , ("pandoc < 2.6", pandocPre26Overrides)
  , ("pandoc >= 2.6 && < 3.1.10", pandocPre3110Overrides) -- https://github.com/jgm/pandoc/commit/55227a20273267c236ec039c3e6559287a1dca45
  , ("pandoc-citeproc", set doCheck False) -- https://github.com/jgm/pandoc-citeproc/issues/369
  , ("purescript", set doCheck False) -- test suite doesn't cope with Nix build env
  , ("proto-lens-protobuf-types", set (libraryDepends . tool . contains (pkg "protobuf")) True)
  , ("proto-lens-protoc", set (libraryDepends . tool . contains (pkg "protobuf")) True)
  , ("qtah-cpp-qt5", set (libraryDepends . system . contains (bind "pkgs.qt5.qtbase")) True)
  , ("qtah-qt5", set (libraryDepends . tool . contains (bind "pkgs.qt5.qtbase")) True)
  , ("readline", over (libraryDepends . system) (Set.union (pkgs ["readline", "ncurses"])))
  , ("req", set doCheck False)  -- test suite requires network access
  , ("rest-rewrite", over (testDepends . system) (Set.union (pkgs ["graphviz", "z3"])))
  , ("sbv > 7", set (testDepends . system . contains (pkg "z3")) True)
  , ("sdr", set (metaSection . platforms) (Just $ Set.singleton (NixpkgsPlatformGroup (ident # "x86_64")))) -- https://github.com/adamwalker/sdr/issues/2
  , ("shake-language-c", set doCheck False) -- https://github.com/samplecount/shake-language-c/issues/26
  , ("ssh", set doCheck False) -- test suite runs forever, probably can't deal with our lack of network access
  , ("stack", set phaseOverrides stackOverrides . set doCheck False)
  , ("stripe-http-streams", set doCheck False . set (metaSection . broken) False)
  , ("target", set (testDepends . system . contains (pkg "z3")) True)
  , ("terminfo", set (libraryDepends . system . contains (pkg "ncurses")) True)
  , ("text", set doCheck False)         -- break infinite recursion
  , ("tensorflow-proto", set (libraryDepends . tool . contains (pkg "protobuf")) True)
  , ("thyme", set (libraryDepends . tool . contains (self "cpphs")) True) -- required on Darwin
  , ("twilio", set doCheck False)         -- attempts to access the network
  , ("udev", set (metaSection . platforms) (Just $ Set.singleton (NixpkgsPlatformGroup (ident # "linux"))))
  , ("websockets", set doCheck False)   -- https://github.com/jaspervdj/websockets/issues/104
  , ("Win32", set (metaSection . platforms) (Just $ Set.singleton (NixpkgsPlatformGroup (ident # "windows"))))
  , ("Win32-shortcut", set (metaSection . platforms) (Just $ Set.singleton (NixpkgsPlatformGroup (ident # "windows"))))
  , ("wxc", wxcHook)
  , ("wxcore", set (libraryDepends . pkgconfig . contains (pkg "wxGTK")) True)
  , ("X11", over (libraryDepends . system) (Set.union (Set.fromList $ map bind ["pkgs.xorg.libXinerama","pkgs.xorg.libXext","pkgs.xorg.libXrender","pkgs.xorg.libXScrnSaver"])))
  , ("xmonad >= 0.14.2", set phaseOverrides xmonadPostInstall)
  , ("zip-archive < 0.3.1", over (testDepends . tool) (replace (self "zip") (pkg "zip")))
  , ("zip-archive >= 0.3.1 && < 0.3.2.3", over (testDepends . tool) (Set.union (Set.fromList [pkg "zip", pkg "unzip"])))   -- https://github.com/jgm/zip-archive/issues/35
  , ("zip-archive >= 0.4", set (testDepends . tool . contains (pkg "which")) True)
  ]

pkg :: Identifier -> Binding
pkg i = binding # (i, path # ["pkgs",i])

self :: Identifier -> Binding
self i = binding # (i, path # ["self",i])

pkgs :: [Identifier] -> Set Binding
pkgs = Set.fromList . map pkg

bind :: String -> Binding
bind s = binding # (i, path # is)
  where
    is = map (review ident) (splitOn "." s)
    i = last is

-- | @replace old new bset@ replaces the Nix binding @old@ with @new@ in the
-- set of bindings @bset@. If @old@ is not found in @bset@, then the function
-- returns @bset@. Unfortunately, the set of 'Binding's may be different
-- between 'Derivation's generated by @cabal2nix@ and @hackage2nix@.
replace :: Binding -> Binding -> Set Binding -> Set Binding
replace old new = Set.map (\x -> if x == old then new else x)

gtk3Hook :: Derivation -> Derivation    -- https://github.com/NixOS/cabal2nix/issues/145
gtk3Hook = set (libraryDepends . pkgconfig . contains (pkg "gtk3")) True
         . over (libraryDepends . pkgconfig) (Set.filter (\b -> view localName b /= "gtk3"))

hfusePreConfigure :: String
hfusePreConfigure = unlines
  [ "preConfigure = ''"
  , "  sed -i -e \"s@  Extra-Lib-Dirs:         /usr/local/lib@  Extra-Lib-Dirs:         ${lib.getLib fuse}/lib@\" HFuse.cabal"
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
  , "preBuild = ''export LD_LIBRARY_PATH=`pwd`/dist/build''${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH'';"
    -- The build step itself, after having built the library, needs to be able
    -- to find the library it just built in order to compile grammar files.
  ]

wxcHook :: Derivation -> Derivation
wxcHook drv = drv & libraryDepends . system %~ Set.union (Set.fromList [pkg "libGL", bind "pkgs.xorg.libX11"])
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
  , "  mkdir -p $out/share/bash-completion"
  , "  mv bash-completion $out/share/bash-completion/completions"
  , "'';"
  ]

darcsInstallPostInstall :: String
darcsInstallPostInstall = unlines
  [ "postInstall = ''"
  , "  mkdir -p $out/share/bash-completion/completions"
  , "  mv contrib/darcs_completion $out/share/bash-completion/completions/darcs"
  , "'';"
  ]

xmonadPostInstall :: String
xmonadPostInstall = unlines
  [ "postInstall = ''"
  , "  install -D man/xmonad.1 ''${!outputDoc}/share/man/man1/xmonad.1"
  , "  install -D man/xmonad.hs ''${!outputDoc}/share/doc/$name/sample-xmonad.hs"
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
  = over (libraryDepends . pkgconfig) (replace (nullBinding (ident # package)) (binding # (ident # package, path # ["pkgs","gst_all_1", ident # package])))

giCairoPhaseOverrides :: Derivation -> Derivation
giCairoPhaseOverrides = over phaseOverrides (++txt)
                      . set (libraryDepends . pkgconfig . contains (pkg "cairo")) True
  where
    txt = unlines [ "preCompileBuildDriver = ''"
                  , "  PKG_CONFIG_PATH+=\":${lib.getDev cairo}/lib/pkgconfig\""
                  , "  setupCompileFlags+=\" $(pkg-config --libs cairo-gobject)\""
                  , "'';"
                  ]

hfseventsOverrides :: Derivation -> Derivation
hfseventsOverrides
  = set isLibrary True
  . set (metaSection . platforms) (Just $ Set.singleton (NixpkgsPlatformGroup (ident # "darwin")))
  . over (libraryDepends . haskell) (Set.union (Set.fromList (map bind ["self.base", "self.cereal", "self.mtl", "self.text", "self.bytestring"])))

opencvOverrides :: Derivation -> Derivation
opencvOverrides = set phaseOverrides "hardeningDisable = [ \"bindnow\" ];"
                . over (libraryDepends . pkgconfig) (replace (pkg "opencv") (pkg "opencv3"))

hspecCoreOverrides :: Derivation -> Derivation   -- https://github.com/hspec/hspec/issues/330
hspecCoreOverrides = set testFlags [ "--skip", "'Test.Hspec.Core.Runner.hspecResult runs specs in parallel'" ]

cabal2nixOverrides :: Derivation -> Derivation
cabal2nixOverrides = set phaseOverrides $ unlines
  [ "preCheck = ''"
  , "  export PATH=\"$PWD/dist/build/cabal2nix:$PATH\""
  , "  export HOME=\"$TMPDIR/home\""
  , "'';"
  ]

gtkglextHook :: Derivation -> Derivation
gtkglextHook = over (libraryDepends . system) (Set.union (Set.fromList deps))
  where
    deps :: [Binding]
    deps = bind <$> [ "pkgs.gtk2"
                    , "pkgs.libGLU"
                    , "pkgs.xorg.libSM"
                    , "pkgs.xorg.libICE"
                    , "pkgs.xorg.libXt"
                    , "pkgs.xorg.libXmu"
                    ]

pandocPre26Overrides :: Derivation -> Derivation
pandocPre26Overrides = set phaseOverrides postInstall
  where
    postInstall = unlines [ "postInstall = ''"
                          , "  mkdir -p $out/share"
                          , "  mv $data/*/*/man $out/share/"
                          , "'';"
                          ]

pandocPre3110Overrides :: Derivation -> Derivation
pandocPre3110Overrides = set phaseOverrides postInstall
  where
    postInstall = unlines [ "postInstall = ''"
                          , "  mkdir -p $out/share/man/man1"
                          , "  mv \"man/\"*.1 $out/share/man/man1/"
                          , "'';"
                          ]

bustleOverrides :: Derivation -> Derivation
bustleOverrides = set (libraryDepends . pkgconfig . contains "system-glib = pkgs.glib") True
                . set (executableDepends . pkgconfig . contains "gio-unix = null") False
                . set (metaSection . license) (Known "lib.licenses.lgpl21Plus")
                . set (metaSection . hydraPlatforms) Nothing

nullBinding :: Identifier -> Binding
nullBinding name = binding # (name, path # [ident # "null"])
