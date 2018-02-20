{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Nixpkgs.Haskell.FromCabal
  ( HaskellResolver, NixpkgsResolver
  , fromGenericPackageDescription , finalizeGenericPackageDescription , fromPackageDescription
  ) where

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
import Distribution.Nixpkgs.Haskell.FromCabal.PostProcess (postProcess)
import qualified Distribution.Nixpkgs.License as Nix
import qualified Distribution.Nixpkgs.Meta as Nix
import Distribution.Package
import Distribution.PackageDescription
import qualified Distribution.PackageDescription as Cabal
import Distribution.PackageDescription.Configuration as Cabal
import Distribution.System
import Distribution.Text ( display )
import Distribution.Types.ComponentRequestedSpec as Cabal
import Distribution.Types.ExeDependency as Cabal
import Distribution.Types.LegacyExeDependency as Cabal
import Distribution.Types.PkgconfigDependency as Cabal
import Distribution.Types.UnqualComponentName as Cabal
import Distribution.Version
import Language.Nix

type HaskellResolver = Dependency -> Bool
type NixpkgsResolver = Identifier -> Maybe Binding

fromGenericPackageDescription :: HaskellResolver -> NixpkgsResolver -> Platform -> CompilerInfo -> FlagAssignment -> [Constraint] -> GenericPackageDescription -> Derivation
fromGenericPackageDescription haskellResolver nixpkgsResolver arch compiler flags constraints genDesc =
  fromPackageDescription haskellResolver nixpkgsResolver missingDeps flags descr
    where
      (descr, missingDeps) = finalizeGenericPackageDescription haskellResolver arch compiler flags constraints genDesc

finalizeGenericPackageDescription :: HaskellResolver -> Platform -> CompilerInfo -> FlagAssignment -> [Constraint] -> GenericPackageDescription -> (PackageDescription, [Dependency])
finalizeGenericPackageDescription haskellResolver arch compiler flags constraints genDesc =
  let
    -- We have to call the Cabal finalizer several times with different resolver
    -- functions, and this convenience function makes our code shorter.
    finalize :: HaskellResolver -> Either [Dependency] (PackageDescription,FlagAssignment)
    finalize resolver = finalizePD flags requestedComponents resolver arch compiler constraints genDesc

    requestedComponents :: ComponentRequestedSpec
    requestedComponents = defaultComponentRequestedSpec
                          { testsRequested      = True
                          , benchmarksRequested = True
                          }

    jailbroken :: HaskellResolver -> HaskellResolver
    jailbroken resolver (Dependency pkg _) = resolver (Dependency pkg anyVersion)

    withInternalLibs :: HaskellResolver -> HaskellResolver
    withInternalLibs resolver d = depPkgName d `elem` internalNames || resolver d

    internalNames :: [PackageName]
    internalNames =    [ unqualComponentNameToPackageName n | (n,_) <- condSubLibraries genDesc ]
                    ++ [ unqualComponentNameToPackageName n | Just n <- libName <$> subLibraries (packageDescription genDesc) ]

  in case finalize (jailbroken (withInternalLibs haskellResolver)) of
    Left m -> case finalize (const True) of
                Left _      -> error ("Cabal cannot finalize " ++ display (packageId genDesc))
                Right (d,_) -> (d,m)
    Right (d,_)  -> (d,[])

fromPackageDescription :: HaskellResolver -> NixpkgsResolver -> [Dependency] -> FlagAssignment -> PackageDescription -> Derivation
fromPackageDescription haskellResolver nixpkgsResolver missingDeps flags PackageDescription {..} = normalize $ postProcess $ nullDerivation
    & isLibrary .~ isJust library
    & pkgid .~ package
    & revision .~ xrev
    & isLibrary .~ isJust library
    & isExecutable .~ not (null executables)
    & extraFunctionArgs .~ mempty
    & libraryDepends .~ foldMap (convertBuildInfo . libBuildInfo) (maybeToList library ++ subLibraries)
    & executableDepends .~ mconcat (map (convertBuildInfo . buildInfo) executables)
    & testDepends .~ mconcat (map (convertBuildInfo . testBuildInfo) testSuites)
    & benchmarkDepends .~ mconcat (map (convertBuildInfo . benchmarkBuildInfo) benchmarks)
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
    & enableSeparateDataOutput .~ not (null dataFiles)
    & subpath .~ "."
    & phaseOverrides .~ mempty
    & editedCabalFile .~ (if xrev > 0
                             then fromMaybe (error (display package ++ ": X-Cabal-File-Hash field is missing")) (lookup "X-Cabal-File-Hash" customFieldsPD)
                             else "")
    & metaSection .~ ( Nix.nullMeta
                     & Nix.homepage .~ homepage
                     & Nix.description .~ synopsis
                     & Nix.license .~ nixLicense
                     & Nix.platforms .~ Nix.allKnownPlatforms
                     & Nix.hydraPlatforms .~ (if nixLicense == Nix.Known "stdenv.lib.licenses.unfree" then Set.empty else Nix.allKnownPlatforms)
                     & Nix.maintainers .~ mempty
                     & Nix.broken .~ not (null missingDeps)
                     )
  where
    xrev = maybe 0 read (lookup "x-revision" customFieldsPD)

    nixLicense :: Nix.License
    nixLicense = fromCabalLicense license

    resolveInHackage :: Identifier -> Binding
    resolveInHackage i | (i^.ident) `elem` [ unPackageName n | (Dependency n _) <- missingDeps ] = bindNull i
                       | otherwise = binding # (i, path # ["self",i])   -- TODO: "self" shouldn't be hardcoded.

    goodScopes :: Set [Identifier]
    goodScopes = Set.fromList (map ("pkgs":) [[], ["xorg"], ["xlibs"], ["gnome2"], ["gnome"], ["gnome3"], ["kde4"]])

    resolveInNixpkgs :: Identifier -> Binding
    resolveInNixpkgs i
      | i `elem` ["clang","lldb","llvm"] = binding # (i, path # ["self","llvmPackages",i])     -- TODO: evil!
      | i == "gtk2"                      = binding # (i, path # ["pkgs","gnome2","gtk"])
      | i == "gtk3"                      = binding # (i, path # ["pkgs","gnome3","gtk"])
      | i == "gtksourceview3"            = binding # (i, path # ["pkgs","gnome3","gtksourceview"])
      | Just p <- nixpkgsResolver i, init (view (reference . path) p) `Set.member` goodScopes = p
      | otherwise                        = bindNull i

    resolveInHackageThenNixpkgs :: Identifier -> Binding
    resolveInHackageThenNixpkgs i | haskellResolver (Dependency (mkPackageName (i^.ident)) anyVersion) = resolveInHackage i
                                  | otherwise = resolveInNixpkgs i

    internalLibNames :: [PackageName]
    internalLibNames = fmap unqualComponentNameToPackageName . catMaybes $ libName <$> subLibraries

    convertBuildInfo :: Cabal.BuildInfo -> Nix.BuildInfo
    convertBuildInfo Cabal.BuildInfo {..} | not buildable = mempty
    convertBuildInfo Cabal.BuildInfo {..} = mempty
      & haskell .~ Set.fromList [ resolveInHackage (toNixName x) | (Dependency x _) <- targetBuildDepends, x `notElem` internalLibNames ]
      & system .~ Set.fromList [ resolveInNixpkgs y | x <- extraLibs, y <- libNixName x ]
      & pkgconfig .~ Set.fromList [ resolveInNixpkgs y | PkgconfigDependency x _ <- pkgconfigDepends, y <- libNixName (unPkgconfigName x) ]
      & tool .~ Set.fromList (map resolveInHackageThenNixpkgs . concatMap buildToolNixName
              $ [ unPackageName x | ExeDependency x _ _ <- buildToolDepends ] ++ [ x | LegacyExeDependency x _ <- buildTools ])

    convertSetupBuildInfo :: Cabal.SetupBuildInfo -> Nix.BuildInfo
    convertSetupBuildInfo bi = mempty
      & haskell .~ Set.fromList [ resolveInHackage (toNixName x) | (Dependency x _) <- Cabal.setupDepends bi ]

bindNull :: Identifier -> Binding
bindNull i = binding # (i, path # ["null"])
