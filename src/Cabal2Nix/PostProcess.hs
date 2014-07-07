{-# LANGUAGE RecordWildCards #-}

module Cabal2Nix.PostProcess ( postProcess ) where

import Distribution.NixOS.Derivation.Cabal
import Data.List

postProcess :: Derivation -> Derivation
postProcess deriv@(MkDerivation {..})
  | pname == "aeson" && version > Version [0,7] []
                                = deriv { buildDepends = "blazeBuilder":buildDepends }
  | pname == "Agda"             = deriv { buildTools = "emacs":buildTools, phaseOverrides = agdaPostInstall }
  | pname == "alex" && version < Version [3,1] []
                                = deriv { buildTools = "perl":buildTools }
  | pname == "alex" && version >= Version [3,1] []
                                = deriv { buildTools = "perl":"happy":buildTools }
  | pname == "bindings-GLFW"    = deriv { extraLibs = "libXext":"libXfixes":extraLibs }
  | pname == "cabal2nix"        = deriv { doCheck = True, phaseOverrides = cabal2nixDoCheckHook }
  | pname == "cabal-install" && version >= Version [0,14] []
                                = deriv { phaseOverrides = cabalInstallPostInstall }
  | pname == "cairo"            = deriv { extraLibs = "pkgconfig":"libc":"cairo":"zlib":extraLibs }
  | pname == "cuda"             = deriv { phaseOverrides = cudaConfigurePhase, extraLibs = "cudatoolkit":"nvidia_x11":"self.stdenv.gcc":extraLibs }
  | pname == "darcs"            = deriv { phaseOverrides = darcsInstallPostInstall }
  | pname == "dns"              = deriv { testTarget = "spec" }
  | pname == "doctest"          = deriv { runHaddock = True, phaseOverrides = doctestNoHaddock }
  | pname == "editline"         = deriv { extraLibs = "libedit":extraLibs }
  | pname == "epic"             = deriv { extraLibs = "gmp":"boehmgc":extraLibs, buildTools = "happy":buildTools }
  | pname == "either"           = deriv { runHaddock = True, phaseOverrides = eitherNoHaddock }
  | pname == "ghc-heap-view"    = deriv { phaseOverrides = ghciPostInstall }
  | pname == "ghc-mod"          = deriv { phaseOverrides = ghcModPostInstall, buildTools = "emacs":buildTools }
  | pname == "ghc-paths"        = deriv { phaseOverrides = ghcPathsPatches }
  | pname == "ghc-vis"          = deriv { phaseOverrides = ghciPostInstall }
  | pname == "git-annex"        = deriv { phaseOverrides = gitAnnexOverrides, buildTools = "git":"rsync":"gnupg1":"curl":"lsof":"openssh":"which":"bup":"perl":buildTools }
  | pname == "github-backup"    = deriv { buildTools = "git":buildTools }
  | pname == "glade"            = deriv { extraLibs = "pkgconfig":"libc":extraLibs, pkgConfDeps = "gtkC":delete "gtk" pkgConfDeps }
  | pname == "glib"             = deriv { extraLibs = "pkgconfig":"libc":extraLibs }
  | pname == "gloss-raster"     = deriv { extraLibs = "llvm":extraLibs }
  | pname == "GLUT"             = deriv { extraLibs = "glut":"libSM":"libICE":"libXmu":"libXi":"mesa":extraLibs }
  | pname == "gtk"              = deriv { extraLibs = "pkgconfig":"libc":extraLibs, buildDepends = delete "gio" buildDepends }
  | pname == "gtkglext"         = deriv { pkgConfDeps = "pangox_compat":pkgConfDeps }
  | pname == "gtk2hs-buildtools"= deriv { buildDepends = "hashtables":buildDepends }
  | pname == "gtksourceview2"   = deriv { extraLibs = "pkgconfig":"libc":extraLibs }
  | pname == "haddock" && version < Version [2,14] []
                                = deriv { buildTools = "alex":"happy":buildTools }
  | pname == "happy"            = deriv { buildTools = "perl":buildTools }
  | pname == "haskeline"        = deriv { buildDepends = "utf8String":buildDepends }
  | pname == "haskell-src"      = deriv { buildTools = "happy":buildTools }
  | pname == "haskell-src-meta" = deriv { buildDepends = "uniplate":buildDepends }
  | pname == "hflags"           = deriv { metaSection = metaSection { license = Unknown (Just "Apache-2.0") } }
  | pname == "hfsevents"        = deriv { buildTools = "gccApple":buildTools, phaseOverrides = "configureFlags = \"--ghc-option=-pgmc=${gccApple}/bin/gcc\";" }
  | pname == "HFuse"            = deriv { phaseOverrides = hfusePreConfigure }
  | pname == "highlighting-kate"= highlightingKatePostProcessing deriv
  | pname == "HList"            = deriv { buildTools = "diffutils":buildTools }
  | pname == "hmatrix"          = deriv { extraLibs = "gsl":"liblapack":"blas":extraLibs }
  | pname == "hoogle"           = deriv { testTarget = "--test-option=--no-net" }
  | pname == "hspec"            = deriv { doCheck = False }
  | pname == "HTTP" && version >= Version [4000,2,14] []
                                = deriv { runHaddock = True, phaseOverrides = httpNoHaddock }
  | pname == "GlomeVec"         = deriv { buildTools = "llvm":buildTools }
  | pname == "idris"            = deriv { buildTools = "happy":buildTools, extraLibs = "gmp":"boehmgc":extraLibs }
  | pname == "language-c-quote" = deriv { buildTools = "alex":"happy":buildTools }
  | pname == "language-java"    = deriv { buildDepends = "syb":buildDepends }
  | pname == "leksah-server"    = deriv { buildDepends = "process-leksah":buildDepends }
  | pname == "lhs2tex"          = deriv { extraLibs = "texLive":extraLibs, phaseOverrides = lhs2texPostInstall }
  | pname == "libffi"           = deriv { extraLibs = delete "ffi" extraLibs }
  | pname == "llvm-base"        = deriv { extraLibs = "llvm":extraLibs }
  | pname == "llvm-general"     = deriv { doCheck = False }
  | pname == "llvm-general-pure"= deriv { doCheck = False }
  | pname == "MFlow"            = deriv { buildTools = "cpphs":buildTools }
  | pname == "markdown-unlit"   = deriv { runHaddock = True, phaseOverrides = markdownUnlitNoHaddock }
  | pname == "multiarg"         = deriv { buildDepends = "utf8String":buildDepends }
  | pname == "mime-mail"        = deriv { extraFunctionArgs = ["sendmail ? \"sendmail\""], phaseOverrides = mimeMailConfigureFlags }
  | pname == "mysql"            = deriv { buildTools = "mysqlConfig":buildTools, extraLibs = "zlib":extraLibs }
  | pname == "ncurses"          = deriv { phaseOverrides = ncursesPatchPhase }
  | pname == "OpenAL"           = deriv { extraLibs = "openal":extraLibs }
  | pname == "OpenGL"           = deriv { extraLibs = "mesa":"libX11":extraLibs }
  | pname == "pandoc"           = deriv { buildDepends = "alex":"happy":buildDepends }
  | pname == "pango"            = deriv { extraLibs = "pkgconfig":"libc":extraLibs }
  | pname == "pcap"             = deriv { extraLibs = "libpcap":extraLibs }
  | pname == "persistent"       = deriv { extraLibs = "sqlite3":extraLibs }
  | pname == "poppler"          = deriv { extraLibs = "libc":extraLibs }
  | pname == "QuickCheck" && version >= Version [2,7,3] []
                                = deriv { runHaddock = True, phaseOverrides = quickCheckNoHaddock }
  | pname == "repa-algorithms"  = deriv { extraLibs = "llvm":extraLibs }
  | pname == "repa-examples"    = deriv { extraLibs = "llvm":extraLibs }
  | pname == "SDL-image"        = deriv { extraLibs = "SDL_image":extraLibs }
  | pname == "SDL-mixer"        = deriv { extraLibs = "SDL_mixer":extraLibs }
  | pname == "SDL-ttf"          = deriv { extraLibs = "SDL_ttf":extraLibs }
  | pname == "sloane"           = deriv { phaseOverrides = sloanePostInstall }
  | pname == "split" && version == Version [0,2,2] []
                                = deriv { doCheck = True, phaseOverrides = splitDoCheck }
  | pname == "structured-haskell-mode" = deriv { buildTools = "emacs":buildTools, phaseOverrides = structuredHaskellModePostInstall }
  | pname == "svgcairo"         = deriv { extraLibs = "libc":extraLibs }
  | pname == "syb" && version == Version [0,4,2] []
                                = deriv { doCheck = True, phaseOverrides = sybDoCheck }
  | pname == "tar"              = deriv { runHaddock = True, phaseOverrides = tarNoHaddock }
  | pname == "terminfo"         = deriv { extraLibs = "ncurses":extraLibs }
  | pname == "text-icu"         = deriv { doCheck = True, phaseOverrides = textIcuDoCheckHook }
  | pname == "threadscope"      = deriv { configureFlags = "--ghc-options=-rtsopts":configureFlags }
  | pname == "transformers" && version >= Version [0,4,1] []
                                = deriv { runHaddock = True, phaseOverrides = transformersNoHaddock }
  | pname == "tz"               = deriv { extraFunctionArgs = ["pkgs_tzdata"], phaseOverrides = "preConfigure = \"export TZDIR=${pkgs_tzdata}/share/zoneinfo\";" }
  | pname == "unix-time"        = deriv { phaseOverrides = unixTimeConfigureFlags }
  | pname == "vacuum"           = deriv { extraLibs = "ghcPaths":extraLibs }
  | pname == "wxc"              = deriv { extraLibs = "wxGTK":"mesa":"libX11":extraLibs, phaseOverrides = wxcPostInstall }
  | pname == "wxcore"           = deriv { extraLibs = "wxGTK":"mesa":"libX11":extraLibs }
  | pname == "X11" && version >= Version [1,6] []
                                = deriv { extraLibs = "libXinerama":"libXext":"libXrender":extraLibs }
  | pname == "X11"              = deriv { extraLibs = "libXinerama":"libXext":extraLibs }
  | pname == "X11-xft"          = deriv { extraLibs = "pkgconfig":"freetype":"fontconfig":extraLibs
                                        , configureFlags = "--extra-include-dirs=${freetype}/include/freetype2":configureFlags
                                        }
  | pname == "xmonad"           = deriv { phaseOverrides = xmonadPostInstall }
  | otherwise                   = deriv

cudaConfigurePhase :: String
cudaConfigurePhase = unlines
  [ "# Perhaps this should be the default in cabal.nix ..."
  , "#"
  , "# The cudatoolkit provides both 64 and 32-bit versions of the"
  , "# library. GHC's linker fails if the wrong version is found first."
  , "# We solve this by eliminating lib64 from the path on 32-bit"
  , "# platforms and putting lib64 first on 64-bit platforms."
  , "libPaths = if self.stdenv.is64bit then \"lib64 lib\" else \"lib\";"
  , "configurePhase = ''"
  , "  for i in Setup.hs Setup.lhs; do"
  , "    test -f $i && ghc --make $i"
  , "  done"
  , "  for p in $extraBuildInputs $propagatedNativeBuildInputs; do"
  , "    if [ -d \"$p/include\" ]; then"
  , "      extraLibDirs=\"$extraLibDirs --extra-include-dir=$p/include\""
  , "    fi"
  , "    for d in $libPaths; do"
  , "      if [ -d \"$p/$d\" ]; then"
  , "        extraLibDirs=\"$extraLibDirs --extra-lib-dir=$p/$d\""
  , "      fi"
  , "    done"
  , "  done"
  , "  ./Setup configure --verbose --prefix=\"$out\" $libraryProfiling $extraLibDirs $configureFlags"
  , "'';"
  ]

ghcModPostInstall :: String
ghcModPostInstall = unlines
  [ "configureFlags = \"--datasubdir=${self.pname}-${self.version}\";"
  , "postInstall = ''"
  , "  cd $out/share/$pname-$version"
  , "  make"
  , "  rm Makefile"
  , "  cd .."
  , "  ensureDir \"$out/share/emacs\""
  , "  mv $pname-$version emacs/site-lisp"
  , "  mv $out/bin/ghc-mod $out/bin/.ghc-mod-wrapped"
  , "  cat - > $out/bin/ghc-mod <<EOF"
  , "  #! ${self.stdenv.shell}"
  , "  COMMAND=\\$1"
  , "  shift"
  , "  eval exec $out/bin/.ghc-mod-wrapped \\$COMMAND \\$( ${self.ghc.GHCGetPackages} ${self.ghc.version} | tr \" \" \"\\n\" | tail -n +2 | paste -d \" \" - - | sed 's/.*/-g \"&\"/' | tr \"\\n\" \" \") \"\\$@\""
  , "  EOF"
  , "  chmod +x $out/bin/ghc-mod"
  , "'';"
  ]

wxcPostInstall :: String
wxcPostInstall = unlines
  [ "postInstall = ''"
  , "  cp -v dist/build/libwxc.so.${self.version} $out/lib/libwxc.so"
  , "'';"
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

highlightingKatePostProcessing :: Derivation -> Derivation
highlightingKatePostProcessing deriv@(MkDerivation {..}) = deriv
  { phaseOverrides = "prePatch = \"sed -i -e 's|regex-pcre-builtin >= .*|regex-pcre|' highlighting-kate.cabal\";"
  , buildDepends = "regex-pcre" : filter (/="regex-pcre-builtin") buildDepends
  }

xmonadPostInstall :: String
xmonadPostInstall = unlines
  [ "postInstall = ''"
  , "  shopt -s globstar"
  , "  mkdir -p $out/share/man/man1"
  , "  mv \"$out/\"**\"/man/\"*.1 $out/share/man/man1/"
  , "'';"
  ]

gitAnnexOverrides :: String
gitAnnexOverrides = unlines
  [ "preConfigure = ''"
  , "  export HOME=\"$NIX_BUILD_TOP/tmp\""
  , "  mkdir \"$HOME\""
  , "'';"
  , "installPhase = \"./Setup install\";"
  , "checkPhase = ''"
  , "  cp dist/build/git-annex/git-annex git-annex"
  , "  ./git-annex test"
  , "'';"
  , "propagatedUserEnvPkgs = [git lsof];"
  ]

ghciPostInstall :: String
ghciPostInstall = unlines
  [ "postInstall = ''"
  , "  ensureDir \"$out/share/ghci\""
  , "  ln -s \"$out/share/$pname-$version/ghci\" \"$out/share/ghci/$pname\""
  , "'';"
  ]

hfusePreConfigure :: String
hfusePreConfigure = unlines
  [ "preConfigure = ''"
  , "  sed -i -e \"s@  Extra-Lib-Dirs:         /usr/local/lib@  Extra-Lib-Dirs:         ${fuse}/lib@\" HFuse.cabal"
  , "  sed -i -e \"s/LANGUAGE FlexibleContexts/LANGUAGE FlexibleContexts, RankNTypes/\" System/Fuse.hsc"
  , "  sed -i -e \"s/E(Exception/E(catch, Exception, IOException/\" System/Fuse.hsc"
  , "  sed -i -e \"s/IO(catch,/IO(/\" System/Fuse.hsc"
  , "  sed -i -e \"s/IO.catch/ E.catch/\" System/Fuse.hsc"
  , "  sed -i -e \"s/const exitFailure/\\\\\\\\(_ :: IOException) -> exitFailure/\" System/Fuse.hsc"
  , "'';"
  ]

ghcPathsPatches :: String
ghcPathsPatches = "patches = [ ./ghc-paths-nix.patch ];"

lhs2texPostInstall :: String
lhs2texPostInstall = unlines
  [ "postInstall = ''"
  , "  mkdir -p \"$out/share/doc/$name\""
  , "  cp doc/Guide2.pdf $out/share/doc/$name"
  , "  mkdir -p \"$out/nix-support\""
  , "'';"
  ]

ncursesPatchPhase :: String
ncursesPatchPhase = "patchPhase = \"find . -type f -exec sed -i -e 's|ncursesw/||' {} \\\\;\";"

doctestNoHaddock, markdownUnlitNoHaddock :: String
markdownUnlitNoHaddock = "noHaddock = self.stdenv.lib.versionOlder self.ghc.version \"7.4\";"
doctestNoHaddock = markdownUnlitNoHaddock

cabal2nixDoCheckHook :: String
cabal2nixDoCheckHook = "doCheck = self.stdenv.lib.versionOlder \"7.6\" self.ghc.version;"

textIcuDoCheckHook :: String
textIcuDoCheckHook = "doCheck = !self.stdenv.isDarwin;"

eitherNoHaddock :: String
eitherNoHaddock = "noHaddock = self.stdenv.lib.versionOlder self.ghc.version \"7.6\";"

httpNoHaddock, quickCheckNoHaddock, tarNoHaddock :: String
httpNoHaddock = "noHaddock = self.stdenv.lib.versionOlder self.ghc.version \"6.11\";"
quickCheckNoHaddock = httpNoHaddock
tarNoHaddock = httpNoHaddock
transformersNoHaddock = httpNoHaddock

agdaPostInstall :: String
agdaPostInstall = unlines
  [ "postInstall = ''"
  , "  $out/bin/agda -c --no-main $out/share/Agda-*/lib/prim/Agda/Primitive.agda"
  , "  $out/bin/agda-mode compile"
  , "'';"
  ]

structuredHaskellModePostInstall :: String
structuredHaskellModePostInstall = unlines
  [ "postInstall = ''"
  , "  emacs -L elisp --batch -f batch-byte-compile \"elisp/\"*.el"
  , "  install -d $out/share/emacs/site-lisp"
  , "  install \"elisp/\"*.el \"elisp/\"*.elc  $out/share/emacs/site-lisp"
  , "'';"
  ]

unixTimeConfigureFlags :: String
unixTimeConfigureFlags =
  "configureFlags = self.stdenv.lib.optionalString self.enableSharedLibraries \"--ghc-option=-fPIC\";"

sloanePostInstall :: String
sloanePostInstall = unlines
  [ "postInstall = ''"
  , "  mkdir -p $out/share/man/man1"
  , "  cp sloane.1 $out/share/man/man1/"
  , "'';"
  ]

mimeMailConfigureFlags :: String
mimeMailConfigureFlags = unlines
  [ "configureFlags = \"--ghc-option=-DMIME_MAIL_SENDMAIL_PATH=\\\"${sendmail}\\\"\";"
  ]

sybDoCheck, splitDoCheck :: String
sybDoCheck = "doCheck = self.stdenv.lib.versionOlder self.ghc.version \"7.9\";"
splitDoCheck = sybDoCheck
