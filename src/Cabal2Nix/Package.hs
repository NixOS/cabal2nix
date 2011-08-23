module Cabal2Nix.Package ( cabal2nix ) where

import Cabal2Nix.License
import Cabal2Nix.PostProcess
import Cabal2Nix.Normalize
import Cabal2Nix.Flags
import Data.Maybe
import Distribution.Compiler
import qualified Distribution.Package as Cabal
import qualified Distribution.PackageDescription as Cabal
import Distribution.PackageDescription.Configuration
import Distribution.System
import Distribution.Version
import Distribution.NixOS.Derivation.Cabal

cabal2nix :: Cabal.GenericPackageDescription -> Derivation
cabal2nix cabal = normalize $ postProcess $ MkDerivation
  { pname        = let Cabal.PackageName x = Cabal.pkgName pkg in x
  , version      = Cabal.pkgVersion pkg
  , sha256       = "cabal2nix left the she256 field undefined"
  , isLibrary    = isJust (Cabal.library tpkg)
  , isExecutable = not (null (Cabal.executables tpkg))
  , buildDepends = map unDep deps
  , buildTools   = map unDep tools
  , extraLibs    = libs
  , pkgConfDeps  = pcs
  , runHaddock   = True
  , metaSection  = Meta
                   { homepage    = Cabal.homepage descr
                   , description = Cabal.synopsis descr
                   , license     = fromCabalLicense (Cabal.license descr)
                   , platforms   = []
                   , maintainers = []
                   }
  }
  where
    descr = Cabal.packageDescription cabal
    pkg   = Cabal.package descr
    deps    = Cabal.buildDepends tpkg
    libDeps = map Cabal.libBuildInfo (maybeToList (Cabal.library tpkg))
    exeDeps = map Cabal.buildInfo (Cabal.executables tpkg)
    tools   = concatMap Cabal.buildTools (libDeps ++ exeDeps)
    libs    = concatMap Cabal.extraLibs (libDeps ++ exeDeps)
    pcs     = map unDep (concatMap Cabal.pkgconfigDepends (libDeps ++ exeDeps))
    Right (tpkg, _) = finalizePackageDescription
                        (configureFlags pkg)
                        (const True)
                        (Platform I386 Linux)                   -- shouldn't be hardcoded
                        (CompilerId GHC (Version [7,0,4] []))   -- dito
                        [] cabal

unDep :: Cabal.Dependency -> String
unDep (Cabal.Dependency (Cabal.PackageName x) _) = x
