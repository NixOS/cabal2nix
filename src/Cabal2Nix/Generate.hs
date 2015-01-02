module Cabal2Nix.Generate ( cabal2nix, cabal2nix' ) where

import Cabal2Nix.Flags
import Cabal2Nix.License
import Cabal2Nix.Normalize
import Cabal2Nix.PostProcess
import Data.Maybe
import Distribution.Compiler
import Distribution.NixOS.Derivation.Cabal
import qualified Distribution.Package as Cabal
import qualified Distribution.PackageDescription as Cabal
import Distribution.PackageDescription.Configuration
import Distribution.System

cabal2nix :: Cabal.GenericPackageDescription -> Derivation
cabal2nix cabal = cabal2nix' descr
  where Right (descr, _) = finalizePackageDescription
                            (configureCabalFlags (Cabal.package (Cabal.packageDescription cabal)))
                            (const True)
                            (Platform X86_64 Linux)                 -- shouldn't be hardcoded
                            (CompilerId GHC (Version [7,8,4] []))   -- dito
                            []
                            cabal

cabal2nix' :: Cabal.PackageDescription -> Derivation
cabal2nix' tpkg = normalize $ postProcess MkDerivation
  { pname          = let Cabal.PackageName x = Cabal.pkgName pkg in x
  , version        = Cabal.pkgVersion pkg
  , revision       = maybe 0 read (lookup "x-revision" xfields)
  , src            = error "cabal2nix left the src field undefined"
  , isLibrary      = isJust (Cabal.library tpkg)
  , isExecutable   = not (null (Cabal.executables tpkg))
  , extraFunctionArgs = []
  , buildDepends   = map unDep deps
  , testDepends    = map unDep tstDeps ++ concatMap Cabal.extraLibs tests
  , buildTools     = map unDep tools
  , extraLibs      = libs
  , pkgConfDeps    = pcs
  , configureFlags = []
  , cabalFlags     = configureCabalFlags pkg
  , runHaddock     = True
  , jailbreak      = False
  , doCheck        = True
  , testTarget     = ""
  , hyperlinkSource = True
  , enableSplitObjs = True
  , phaseOverrides = ""
  , editedCabalFile= if isJust (lookup "x-revision" xfields) then fromJust (lookup "x-cabal-file-hash" xfields) else ""
  , metaSection    = Meta
                   { homepage       = Cabal.homepage tpkg
                   , description    = Cabal.synopsis tpkg
                   , license        = fromCabalLicense (Cabal.license tpkg)
                   , platforms      = []
                   , hydraPlatforms = []
                   , maintainers    = []
                   , broken         = False
                   }
  }
  where
    xfields = Cabal.customFieldsPD tpkg
    pkg     = Cabal.package tpkg
    deps    = Cabal.buildDepends tpkg
    tests   = map Cabal.testBuildInfo (Cabal.testSuites tpkg)
    libDeps = map Cabal.libBuildInfo (maybeToList (Cabal.library tpkg))
    exeDeps = map Cabal.buildInfo (Cabal.executables tpkg)
    tstDeps = concatMap Cabal.buildTools tests ++ concatMap Cabal.pkgconfigDepends tests ++
              concatMap Cabal.targetBuildDepends tests
    tools   = concatMap Cabal.buildTools (libDeps ++ exeDeps)
    libs    = concatMap Cabal.extraLibs (libDeps ++ exeDeps)
    pcs     = map unDep (concatMap Cabal.pkgconfigDepends (libDeps ++ exeDeps))

unDep :: Cabal.Dependency -> String
unDep (Cabal.Dependency (Cabal.PackageName x) _) = x
