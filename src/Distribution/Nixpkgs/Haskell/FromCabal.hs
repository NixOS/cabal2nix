{-# LANGUAGE RecordWildCards #-}

module Distribution.Nixpkgs.Haskell.FromCabal ( cabal2nix, cabal2nix' ) where

import Distribution.Nixpkgs.Haskell.FromCabal.Flags
import Distribution.Nixpkgs.Haskell.FromCabal.License
import Distribution.Nixpkgs.Haskell.FromCabal.Name
import Distribution.Nixpkgs.Haskell.FromCabal.Normalize
import Distribution.Nixpkgs.Haskell.FromCabal.PostProcess
import Internal.Lens
import Data.Maybe
import qualified Data.Set as Set
import Data.Version
import Distribution.Compiler
import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.Haskell
import qualified Distribution.Nixpkgs.Haskell as Nix
import qualified Distribution.Nixpkgs.Meta as Nix
import Distribution.Package
import Distribution.PackageDescription
import qualified Distribution.PackageDescription as Cabal
import Distribution.PackageDescription.Configuration
import Distribution.System
import Distribution.Text ( display )

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

cabal2nix' :: PackageDescription -> Derivation
cabal2nix' PackageDescription {..} = normalize $ postProcess $
  let
    xrev = maybe 0 read (lookup "x-revision" customFieldsPD)
  in
  def
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
  & editedCabalFile .~ (if xrev > 0 then fromMaybe (error (display package ++ ": X-Cabal-File-Hash field is missing"))
                                                   (lookup "X-Cabal-File-Hash" customFieldsPD)
                                    else "")
  & metaSection .~ ( def
                   & Nix.homepage .~ homepage
                   & Nix.description .~ synopsis
                   & Nix.license .~ fromCabalLicense license
                   & Nix.platforms .~ mempty
                   & Nix.hydraPlatforms .~ mempty
                   & Nix.maintainers .~ mempty
                   & Nix.broken .~ False
                   )

convertBuildInfo :: Cabal.BuildInfo -> Nix.BuildInfo
convertBuildInfo Cabal.BuildInfo {..} = mempty
  & haskell .~ Set.fromList [ toNixName x | (Dependency x _) <- targetBuildDepends ]
  & system .~ Set.fromList [ y | x <- extraLibs, y <- libNixName x ]
  & pkgconfig .~ Set.fromList [ y | Dependency (PackageName x) _ <- pkgconfigDepends, y <- libNixName x ]
  & tool .~ Set.fromList [ y | Dependency (PackageName x) _ <- buildTools, y <- buildToolNixName x ]
