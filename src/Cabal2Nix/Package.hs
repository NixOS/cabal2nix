module Cabal2Nix.Package ( cabal2nix ) where

import Data.Maybe
import Distribution.Compiler
import qualified Distribution.Package as Cabal
import qualified Distribution.PackageDescription as Cabal
import Distribution.PackageDescription.Configuration
import Distribution.System
import Distribution.Version
import Distribution.NixOS.Derivation.Cabal
import Cabal2Nix.Name
import Cabal2Nix.CorePackages
import Data.List

cabal2nix :: Cabal.GenericPackageDescription -> Derivation
cabal2nix cabal = MkDerivation
  { pname        = Cabal.pkgName pkg
  , version      = Cabal.pkgVersion pkg
  , sha256       = "cabal2nix left the she256 field undefined"
  , isLibrary    = isJust (Cabal.library tpkg)
  , isExecutable = not (null (Cabal.executables tpkg))
  , buildDepends = pkgDeps
  , buildTools   = pkgBuildTools
  , extraLibs    = pkgLibs
  , pkgConfDeps  = pkgPCs
  , runHaddock   = True
  , metaSection  = Meta
                   { homepage    = Cabal.homepage descr
                   , description = normalizeDescription (Cabal.synopsis descr)
                   , license     = fromCabalLicense (Cabal.license descr)
                   , platforms   = []
                   , maintainers = []
                   }
  }
  where
    descr = Cabal.packageDescription cabal
    pkg   = Cabal.package descr
    Cabal.PackageName name  = Cabal.pkgName pkg
    deps    = (Cabal.buildDepends tpkg)
    libDeps = map Cabal.libBuildInfo $ maybeToList (Cabal.library tpkg)
    exeDeps = map    Cabal.buildInfo $ Cabal.executables tpkg
    tools   =             concatMap Cabal.buildTools       (libDeps ++ exeDeps)
    libs    =             concatMap Cabal.extraLibs        (libDeps ++ exeDeps)
    pcs     = map unDep $ concatMap Cabal.pkgconfigDepends (libDeps ++ exeDeps)
    Right (tpkg, _) = finalizePackageDescription [] (const True)
                        (Platform I386 Linux) -- shouldn't be hardcoded
                        (CompilerId GHC (Version [7,0,4] [])) -- dito
                        [] cabal
    pkgLibs       = nub $ sort $ concatMap libNixName libs
    pkgPCs        = nub $ sort $ concatMap libNixName pcs
    pkgDeps       = nub $ sort $ map toNixName $
                     filter (`notElem` (name : corePackages)) $ map unDep deps
    pkgBuildTools = nub $ sort $ map toNixName $
                     filter (`notElem` coreBuildTools) $ map unDep tools

unDep :: Cabal.Dependency -> String
unDep (Cabal.Dependency (Cabal.PackageName x) _) = x

normalizeDescription :: String -> String
normalizeDescription desc
  | null desc                                             = []
  | last desc == '.' && length (filter ('.'==) desc) == 1 = normalizeDescription (reverse (tail (reverse desc)))
  | otherwise                                             = unwords (words desc) >>= quote

quote :: Char -> [Char]
quote '"'  = "\\\""
quote c    = [c]
