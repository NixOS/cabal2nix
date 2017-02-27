{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Nixpkgs.Haskell.FromCabal
  ( HaskellResolver, NixpkgsResolver
  , fromGenericPackageDescription , fromPackageDescription
  )
  where

import Control.Arrow ( second )
import Control.Lens
import Data.Maybe
import Data.Set ( Set )
import qualified Data.Set as Set
import Distribution.Compiler
import Distribution.Nixpkgs.Haskell
import qualified Distribution.Nixpkgs.Haskell as Nix
import Distribution.Nixpkgs.Haskell.Constraint
import Distribution.Nixpkgs.Haskell.FromCabal.License
import Distribution.Nixpkgs.Haskell.FromCabal.Name
import Distribution.Nixpkgs.Haskell.FromCabal.Normalize
import Distribution.Nixpkgs.Haskell.FromCabal.PostProcess
import qualified Distribution.Nixpkgs.Meta as Nix
import Distribution.Package
import Distribution.PackageDescription
import qualified Distribution.PackageDescription as Cabal
import Distribution.PackageDescription.Configuration
import Distribution.System
import Distribution.Text ( display )
import Distribution.Version
import Language.Nix

type HaskellResolver = Dependency -> Bool
type NixpkgsResolver = Identifier -> Maybe Binding

fromGenericPackageDescription :: HaskellResolver -> NixpkgsResolver -> Platform -> CompilerInfo ->  FlagAssignment -> [Constraint] -> GenericPackageDescription -> Derivation
fromGenericPackageDescription haskellResolver nixpkgsResolver arch compiler flags constraints genDesc =
  fromPackageDescription haskellResolver nixpkgsResolver missingDeps flags descr
    where
      -- We have to call the Cabal finalizer several times with different resolver
      -- functions, and this convenience function makes our code shorter.
      finalize :: HaskellResolver -> Either [Dependency] (PackageDescription,FlagAssignment)
      finalize resolver = finalizePackageDescription flags resolver arch compiler constraints (enableTests genDesc)

      descr :: PackageDescription; missingDeps :: [Dependency]
      (descr,missingDeps) = case finalize jailbrokenResolver of
                              Left m -> case finalize (const True) of
                                          Left _      -> error ("Cabal cannot finalize " ++ display (packageId genDesc))
                                          Right (d,_) -> (d,m)
                              Right (d,_)  -> (d,[])

      jailbrokenResolver :: HaskellResolver
      jailbrokenResolver (Dependency pkg _) = haskellResolver (Dependency pkg anyVersion)

fromPackageDescription :: HaskellResolver -> NixpkgsResolver -> [Dependency] -> FlagAssignment -> PackageDescription -> Derivation
fromPackageDescription haskellResolver nixpkgsResolver missingDeps flags (PackageDescription {..}) = normalize $ postProcess $ nullDerivation
    & isLibrary .~ isJust library
    & pkgid .~ package
    & revision .~ xrev
    & isLibrary .~ isJust library
    & isExecutable .~ not (null executables)
    & extraFunctionArgs .~ mempty
    & libraryDepends .~ maybe mempty (convertBuildInfo . libBuildInfo) library
    & executableDepends .~ mconcat (map (convertBuildInfo . buildInfo) executables)
    & testDepends .~ mconcat (map (convertBuildInfo . testBuildInfo) testSuites)
    & benchDepends .~ mconcat (map (convertBuildInfo . benchmarkBuildInfo) benchmarks)
    & Nix.setupDepends .~ maybe mempty convertSetupBuildInfo setupBuildInfo
    & configureFlags .~ mempty
    & cabalFlags .~ flags
    & runHaddock .~ maybe True (not . null . exposedModules) library
    & jailbreak .~ False
    & doCheck .~ True
    & testTarget .~ mempty
    & hyperlinkSource .~ True
    & enableSplitObjs .~ True
    & enableLibraryProfiling .~ False
    & enableExecutableProfiling .~ False
    & phaseOverrides .~ mempty
    & editedCabalFile .~ (if xrev > 0
                             then fromMaybe (error (display package ++ ": X-Cabal-File-Hash field is missing")) (lookup "X-Cabal-File-Hash" customFieldsPD)
                             else "")
    & metaSection .~ ( Nix.nullMeta
                     & Nix.homepage .~ homepage
                     & Nix.description .~ synopsis
                     & Nix.license .~ fromCabalLicense license
                     & Nix.platforms .~ Nix.allKnownPlatforms
                     & Nix.hydraPlatforms .~ Nix.allKnownPlatforms
                     & Nix.maintainers .~ mempty
                     & Nix.broken .~ not (null missingDeps)
                     )
  where
    xrev = maybe 0 read (lookup "x-revision" customFieldsPD)

    resolveInHackage :: Identifier -> Binding
    resolveInHackage i | (i^.ident) `elem` [ n | (Dependency (PackageName n) _) <- missingDeps ] = bindNull i
                       | otherwise = binding # (i, path # ["self",i])   -- TODO: "self" shouldn't be hardcoded.

    goodScopes :: Set [Identifier]
    goodScopes = Set.fromList (map ("pkgs":) [[], ["xorg"], ["xlibs"], ["gnome2"], ["gnome"], ["gnome3"], ["kde4"]])

    resolveInNixpkgs :: Identifier -> Binding
    resolveInNixpkgs i
      | i `elem` ["clang","lldb","llvm"] = binding # (i, path # ["self","llvmPackages",i])     -- TODO: evil!
      | i == "gtk2"                      = binding # (i, path # ["pkgs","gnome2","gtk"])
      | i == "gtk3"                      = binding # (i, path # ["pkgs","gnome3","gtk"])
      | Just p <- nixpkgsResolver i, init (view (reference . path) p) `Set.member` goodScopes = p
      | otherwise                        = bindNull i

    resolveInHackageThenNixpkgs :: Identifier -> Binding
    resolveInHackageThenNixpkgs i | haskellResolver (Dependency (PackageName (i^.ident)) anyVersion) = resolveInHackage i
                                  | otherwise = resolveInNixpkgs i

    convertBuildInfo :: Cabal.BuildInfo -> Nix.BuildInfo
    convertBuildInfo Cabal.BuildInfo {..} = mempty
      & haskell .~ Set.fromList [ resolveInHackage (toNixName x) | (Dependency x _) <- targetBuildDepends ]
      & system .~ Set.fromList [ resolveInNixpkgs y | x <- extraLibs, y <- libNixName x ]
      & pkgconfig .~ Set.fromList [ resolveInNixpkgs y | Dependency (PackageName x) _ <- pkgconfigDepends, y <- libNixName x ]
      & tool .~ Set.fromList [ resolveInHackageThenNixpkgs y | Dependency (PackageName x) _ <- buildTools, y <- buildToolNixName x ]

    convertSetupBuildInfo :: Cabal.SetupBuildInfo -> Nix.BuildInfo
    convertSetupBuildInfo bi = mempty
      & haskell .~ Set.fromList [ resolveInHackage (toNixName x) | (Dependency x _) <- Cabal.setupDepends bi ]

bindNull :: Identifier -> Binding
bindNull i = binding # (i, path # ["null"])

enableTests :: GenericPackageDescription -> GenericPackageDescription
enableTests gd = gd { condTestSuites = flaggedTests }
  where
    flaggedTests :: [(String, CondTree ConfVar [Dependency] TestSuite)]
    flaggedTests = map (second (mapTreeData enableTest)) (condTestSuites gd)

    enableTest :: TestSuite -> TestSuite
    enableTest t = t { testEnabled = True }
