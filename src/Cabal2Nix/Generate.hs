{-# LANGUAGE RecordWildCards #-}

module Cabal2Nix.Generate ( cabal2nix, cabal2nix' ) where

import Cabal2Nix.Flags
import Cabal2Nix.Name
import Cabal2Nix.Normalize
import Cabal2Nix.PostProcess
import Control.Lens
-- import Data.Set.Lens
-- import Control.Applicative
import Data.Maybe
import qualified Data.Set as Set
import Data.Version
import Distribution.Compiler
import Distribution.Nixpkgs.Haskell
import qualified Distribution.Nixpkgs.Haskell as Nix
import Cabal2Nix.License
import Distribution.Nixpkgs.Fetch
import qualified Distribution.Nixpkgs.Meta as Nix
import Distribution.Package
import Distribution.Version
import Distribution.PackageDescription
import qualified Distribution.PackageDescription as Cabal
import Distribution.PackageDescription.Configuration
import Distribution.System

cabal2nix :: FlagAssignment -> GenericPackageDescription -> Derivation
cabal2nix flags' cabal = normalize $ drv & cabalFlags .~ flags
  where drv = cabal2nix' descr
        flags = normalizeCabalFlags (flags' ++ configureCabalFlags (package (packageDescription cabal)))
        Right (descr, _) = finalizePackageDescription
                            flags
                            (const True)
                            (Platform X86_64 Linux)                 -- shouldn't be hardcoded
                            (unknownCompilerInfo (CompilerId GHC (Version [7,10,2] [])) NoAbiTag)
                            []
                            cabal

nullDerivation :: Derivation
nullDerivation = MkDerivation
  { _pkgid = error "undefined _pkgid"
  , _revision = error "undefined _revision"
  , _src = error "undefined _src"
  , _isLibrary = error "undefined _isLibrary"
  , _isExecutable = error "undefined _isExecutable"
  , _extraFunctionArgs = error "undefined _extraFunctionArgs"
  , _libraryDepends = error "undefined _libraryDepends"
  , _executableDepends = error "undefined _executableDepends"
  , _testDepends = error "undefined _testDepends"
  , _configureFlags = error "undefined _configureFlags"
  , _cabalFlags = error "undefined _cabalFlags"
  , _runHaddock = error "undefined _runHaddock"
  , _jailbreak = error "undefined _jailbreak"
  , _doCheck = error "undefined _doCheck"
  , _testTarget = error "undefined _testTarget"
  , _hyperlinkSource = error "undefined _hyperlinkSource"
  , _enableSplitObjs = error "undefined _enableSplitObjs"
  , _phaseOverrides = error "undefined _phaseOverrides"
  , _editedCabalFile = error "undefined _editedCabalFile"
  , _metaSection = error "undefined _metaSection"
  }

cabal2nix' :: PackageDescription -> Derivation
cabal2nix' PackageDescription {..} = normalize $ postProcess $
  let
    xrev = maybe 0 read (lookup "x-revision" customFieldsPD)
  in
  nullDerivation
  & pkgid .~ package
  & revision .~ xrev
  & src .~ DerivationSource
             { derivKind = "url"
             , derivUrl = mempty
             , derivRevision = mempty
             , derivHash = mempty
             }
  & isLibrary .~ isJust library
  & isExecutable .~ not (null executables)
  & extraFunctionArgs .~ mempty
  & libraryDepends .~ maybe mempty (convertBuildInfo . libBuildInfo) library
  & executableDepends .~ mconcat (map (convertBuildInfo . buildInfo) executables)
  & testDepends .~ mconcat (map (convertBuildInfo . testBuildInfo) testSuites)
  & configureFlags .~ mempty
  & cabalFlags .~ configureCabalFlags package
  & runHaddock .~ True
  & jailbreak .~ False
  & doCheck .~ True
  & testTarget .~ mempty
  & hyperlinkSource .~ True
  & enableSplitObjs .~ True
  & phaseOverrides .~ mempty
  & editedCabalFile .~ (if xrev > 0 then fromJust (lookup "x-cabal-file-hash" customFieldsPD) else "")
  & metaSection .~ ( nullMeta
                   & Nix.homepage .~ homepage
                   & Nix.description .~ synopsis
                   & Nix.license .~ fromCabalLicense license
                   & Nix.platforms .~ mempty
                   & Nix.hydraPlatforms .~ mempty
                   & Nix.maintainers .~ mempty
                   & Nix.broken .~ False
                   )

nullBuildInfo :: Nix.BuildInfo
nullBuildInfo = Nix.BuildInfo
  { _haskell = error "undefined _haskell"
  , _pkgconfig = error "undefined _pkgconfig"
  , _system = error "undefined _system"
  , _tool = error "undefined _tool"
  }

nullMeta :: Nix.Meta
nullMeta = Nix.Meta
 { Nix._homepage = error "undefined Nix._homepage"
 , Nix._description = error "undefined Nix._description"
 , Nix._license = error "undefined Nix._license"
 , Nix._platforms = error "undefined Nix._platforms"
 , Nix._hydraPlatforms = error "undefined Nix._hydraPlatforms"
 , Nix._maintainers = error "undefined Nix._maintainers"
 , Nix._broken = error "undefined Nix._broken"
 }

convertBuildInfo :: Cabal.BuildInfo -> Nix.BuildInfo
convertBuildInfo Cabal.BuildInfo {..} = nullBuildInfo
  & haskell .~ Set.fromList targetBuildDepends
  & system .~ Set.fromList [ Dependency (PackageName y) anyVersion | x <- extraLibs, y <- libNixName x, not (null y) ]
  & pkgconfig .~ Set.fromList [ Dependency (PackageName y) anyVersion | Dependency (PackageName x) _ <- pkgconfigDepends, y <- libNixName x, not (null y) ]
  & tool .~ Set.fromList [ Dependency (PackageName y) anyVersion | Dependency (PackageName x) _ <- buildTools, y <- buildToolNixName x, not (null y) ]
