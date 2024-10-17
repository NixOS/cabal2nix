{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Nixpkgs.Haskell.FromCabal
  ( HaskellResolver, NixpkgsResolver, OutputGranularity (..)
  , fromGenericPackageDescription , finalizeGenericPackageDescription , fromPackageDescription
  ) where

import Control.Lens
import Data.Maybe
import Data.Set ( Set )
import qualified Data.Set as Set
import Distribution.Compiler
import Distribution.Pretty
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
import Distribution.Types.PackageVersionConstraint
import Distribution.Text ( display )
import Distribution.Types.ComponentRequestedSpec as Cabal
import Distribution.Types.ExeDependency as Cabal
import Distribution.Types.LegacyExeDependency as Cabal
import Distribution.Types.PkgconfigDependency as Cabal
import Distribution.Types.UnqualComponentName as Cabal
import Distribution.Utils.ShortText ( fromShortText )
import Distribution.Version
import Language.Nix
import Distribution.Nixpkgs.Fetch (DerivKind(DerivKindHg))
import Text.PrettyPrint (render)

type HaskellResolver = PackageVersionConstraint -> Bool
type NixpkgsResolver = Identifier -> Maybe Binding

data OutputGranularity
  = SingleDerivation
  -- One derivation per target, where libraries are not excluded from executables' libraryDepends
  | PerTarget

fromGenericPackageDescription 
  :: (Derivation -> Derivation)
  -> HaskellResolver
  -> NixpkgsResolver
  -> Platform
  -> CompilerInfo
  -> FlagAssignment
  -> OutputGranularity
  -> [Constraint]
  -> GenericPackageDescription
  -> PackageNix
fromGenericPackageDescription overrideDrv haskellResolver nixpkgsResolver arch compiler flags granularity constraints genDesc =
  fromPackageDescription overrideDrv haskellResolver nixpkgsResolver missingDeps flags granularity descr
    where
      (descr, missingDeps) = finalizeGenericPackageDescription haskellResolver arch compiler flags constraints genDesc

finalizeGenericPackageDescription :: HaskellResolver -> Platform -> CompilerInfo -> FlagAssignment -> [Constraint] -> GenericPackageDescription -> (PackageDescription, [Dependency])
finalizeGenericPackageDescription haskellResolver arch compiler flags constraints genDesc =
  let
    -- finalizePD incooperates the 'LibraryName' of a dependency
    -- which we always ignore, so the Cabal-compatible resolver
    -- is a simple wrapper around our 'HaskellResolver'
    makeCabalResolver :: HaskellResolver -> Dependency -> Bool
    makeCabalResolver r (Dependency n v _) = r (PackageVersionConstraint n v)

    -- the finalizePD API changed in Cabal 3.4.0.0, so we need to do some plumbing.
    -- See https://github.com/haskell/cabal/issues/5570
#if MIN_VERSION_Cabal(3,4,0)
    makeCabalConstraints :: [Constraint] -> [PackageVersionConstraint]
    makeCabalConstraints = id
#else
    makeCabalConstraints :: [Constraint] -> [Dependency]
    makeCabalConstraints = map $ \(PackageVersionConstraint n v) -> Dependency n v mempty
#endif

    -- We have to call the Cabal finalizer several times with different resolver
    -- functions, and this convenience function makes our code shorter.
    finalize :: HaskellResolver -> Either [Dependency] (PackageDescription,FlagAssignment)
    finalize resolver = finalizePD flags requestedComponents (makeCabalResolver resolver) arch compiler (makeCabalConstraints constraints) genDesc

    requestedComponents :: ComponentRequestedSpec
    requestedComponents = ComponentRequestedSpec
                          { testsRequested      = True
                          , benchmarksRequested = True
                          }

    jailbroken :: HaskellResolver -> HaskellResolver
    jailbroken resolver (PackageVersionConstraint pkg _) = resolver (PackageVersionConstraint pkg anyVersion)

    withInternalLibs :: HaskellResolver -> HaskellResolver
    withInternalLibs resolver c = constraintPkgName c `elem` internalNames || resolver c

    internalNames :: [PackageName]
    internalNames =    [ unqualComponentNameToPackageName n | (n,_) <- condSubLibraries genDesc ]
                    ++ [ unqualComponentNameToPackageName n | LSubLibName n <- libName <$> subLibraries (packageDescription genDesc) ]

  in case finalize (jailbroken (withInternalLibs haskellResolver)) of
    Left m -> case finalize (const True) of
                Left _      -> error ("Cabal cannot finalize " ++ display (packageId genDesc))
                Right (d,_) -> (d,m)
    Right (d,_)  -> (d,[])

fromPackageDescription
  :: (Derivation -> Derivation)
  -> HaskellResolver
  -> NixpkgsResolver
  -> [Dependency]
  -> FlagAssignment
  -> OutputGranularity
  -> PackageDescription
  -> PackageNix
fromPackageDescription overrideDrv haskellResolver nixpkgsResolver missingDeps flags granularity packageDescription@PackageDescription {..}
  = case granularity of
    SingleDerivation -> singleDerivationPackage
    PerTarget -> multiDerivationPackage
  where
    singleDerivationPackage :: PackageNix
    singleDerivationPackage = nullSingleDrvPackage 
      & derivation .~ processDrv (baseDerivation
        & isLibrary .~ isJust library
        & isExecutable .~ not (null executables)
        & libraryDepends .~ foldMap (convertSingleDerivationBuildInfo . libBuildInfo) (allLibraries packageDescription)
        & executableDepends .~ mconcat (map (convertSingleDerivationBuildInfo . buildInfo) executables)
        & testDepends .~ mconcat (map (convertSingleDerivationBuildInfo . testBuildInfo) testSuites)
        & benchmarkDepends .~ mconcat (map (convertSingleDerivationBuildInfo . benchmarkBuildInfo) benchmarks))

    processDrv :: Derivation -> Derivation
    processDrv = overrideDrv . normalize . postProcess

    multiDerivationPackage :: PackageNix
    multiDerivationPackage = nullTargetDrvsPackage
      & targetDrvs .~ (nullTargetDerivations
        & libraries .~ fmap (processDrv . toLibraryDerivation) (allLibraries packageDescription)
        & exes .~ fmap (processDrv . toExecutableDerivation) executables
        & testExes .~ fmap (processDrv . toTestDerivation) testSuites
        & benchExes .~ fmap (processDrv . toBenchDerivation) benchmarks)

    toLibraryDerivation :: Library -> Derivation
    toLibraryDerivation lib = baseDerivation
      & pkgid .~ case libName lib of
          LMainLibName -> baseDerivation^.pkgid
          LSubLibName subLibName -> (baseDerivation^.pkgid) { pkgName = unqualComponentNameToPackageName subLibName }
      & isLibrary .~ True
      & isExecutable .~ False
      & doCheck .~ False
      & libraryDepends .~ convertComponentDerivationBuildInfo (libBuildInfo lib)
      & buildTarget .~ render (case libName lib of
          LMainLibName -> pretty $ packageName (baseDerivation^.pkgid)
          name@LSubLibName {} -> prettyLibraryNameComponent name)

    toExecutableDerivation :: Executable -> Derivation
    toExecutableDerivation executable = baseDerivation
      & pkgid .~ (baseDerivation^.pkgid) { pkgName = unqualComponentNameToPackageName (exeName executable) }
      & isLibrary .~ False
      & isExecutable .~ True
      & executableDepends .~ convertComponentDerivationBuildInfo (buildInfo executable)
      & buildTarget .~ unUnqualComponentName (exeName executable)

    toTestDerivation :: TestSuite -> Derivation
    toTestDerivation testSuite = baseDerivation
      & pkgid .~ (baseDerivation^.pkgid) { pkgName = unqualComponentNameToPackageName (testName testSuite) }
      & isLibrary .~ False
      & isExecutable .~ True
      & testDepends .~ convertComponentDerivationBuildInfo (testBuildInfo testSuite)
      & buildTarget .~ unUnqualComponentName (testName testSuite)

    toBenchDerivation :: Benchmark -> Derivation
    toBenchDerivation benchmark = baseDerivation
      & pkgid .~ (baseDerivation^.pkgid) { pkgName = unqualComponentNameToPackageName (benchmarkName benchmark) }
      & isLibrary .~ False
      & isExecutable .~ True
      & doBenchmark .~ True
      & benchmarkDepends .~ convertComponentDerivationBuildInfo (benchmarkBuildInfo benchmark)
      & buildTarget .~ unUnqualComponentName (benchmarkName benchmark)

    baseDerivation :: Derivation
    baseDerivation = nullDerivation
      & pkgid .~ package
      & revision .~ xrev
      & extraFunctionArgs .~ mempty
      & extraAttributes .~ mempty
      & Nix.setupDepends .~ maybe mempty convertSetupBuildInfo setupBuildInfo
      & configureFlags .~ mempty
      & cabalFlags .~ flags
      & runHaddock .~ doHaddockPhase
      & jailbreak .~ False
      & doCheck .~ True
      & doBenchmark .~ False
      & buildTarget .~ mempty
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
#if MIN_VERSION_Cabal(3,2,0)
                       & Nix.homepage .~ stripRedundanceSpaces (fromShortText homepage)
                       & Nix.description .~ stripRedundanceSpaces (fromShortText synopsis)
#else
                       & Nix.homepage .~ stripRedundanceSpaces homepage
                       & Nix.description .~ stripRedundanceSpaces synopsis
#endif
                       & Nix.license .~ nixLicense
                       & Nix.platforms .~ Nothing
                       & Nix.badPlatforms .~ Nothing
                       & Nix.hydraPlatforms .~ (if isFreeLicense nixLicense then Nothing else Just Set.empty)
                       & Nix.mainProgram .~ nixMainProgram
                       & Nix.maintainers .~ mempty
                       & Nix.broken .~ not (null missingDeps)
                       )

    xrev = maybe 0 read (lookup "x-revision" customFieldsPD)

    nixLicense :: Nix.License
    nixLicense =  either fromSPDXLicense fromCabalLicense licenseRaw

    -- return the name of the executable if there is exactly one. If more,
    -- it is hard to decide automatically which should be the default/main one.
    nixMainProgram :: Maybe String
    nixMainProgram =
      case filter (buildable . buildInfo) executables of
        [mainProgram] -> Just $ unUnqualComponentName $ exeName mainProgram
        _ -> Nothing

    resolveInHackage :: Identifier -> Binding
    resolveInHackage i | (i^.ident) `elem` [ unPackageName n | (Dependency n _ _) <- missingDeps ] = bindNull i
                       | otherwise = binding # (i, path # ["self",i])   -- TODO: "self" shouldn't be hardcoded.

    -- TODO: This is all very confusing. Haskell packages refer to the Nixpkgs
    -- derivation 'foo' as 'pkgs.foo', because they live in the 'haskellPackages'
    -- name space -- not on the top level. Therefore, we built our Nixpkgs lookup
    -- function so that top level names are returned as 'pkgs.foo'. As a result, we
    -- end up pre-pending that path to all kinds of names all over the place. I
    -- suppose the correct approach would be to assume that the lookup function
    -- returns names that live in the top-level and to adapt the code in
    -- PostProcess.hs et all to that fact.
    goodScopes :: Set [Identifier]
    goodScopes = Set.fromList (map ("pkgs":) [[], ["xorg"], ["xlibs"], ["gnome2"], ["gnome"], ["gnome3"], ["kde4"]])

    resolveInNixpkgs :: Identifier -> Binding
    resolveInNixpkgs i
      | i `elem` ["clang","lldb","llvm"] = binding # (i, path # ["self","llvmPackages",i])     -- TODO: evil!
      | i == "gtk2"                      = binding # (i, path # ["pkgs","gtk2"])               -- TODO: these cases should not be necessary
      | i == "gtk3"                      = binding # (i, path # ["pkgs","gtk3"])
      | i == "gtksourceview3"            = binding # (i, path # ["pkgs","gtksourceview3"])
      | i == "vte_291"                   = binding # (i, path # ["pkgs","vte"])
      | Just p <- nixpkgsResolver i, init (view (reference . path) p) `Set.member` goodScopes = p
      | otherwise                        = bindNull i

    resolveInHackageThenNixpkgs :: Identifier -> Binding
    resolveInHackageThenNixpkgs i | haskellResolver (PackageVersionConstraint (mkPackageName (i^.ident)) anyVersion) = resolveInHackage i
                                  | otherwise = resolveInNixpkgs i

    internalLibNames :: [PackageName]
    internalLibNames = [ unqualComponentNameToPackageName n | LSubLibName n <- libName <$> subLibraries ]

    doHaddockPhase :: Bool
    doHaddockPhase | not (null internalLibNames) = False
                   | Just l <- library           = not (null (exposedModules l))
                   | otherwise                   = True

    convertSingleDerivationBuildInfo :: Cabal.BuildInfo -> Nix.BuildInfo
    convertSingleDerivationBuildInfo = convertBuildInfo (`notElem` internalLibNames)

    convertComponentDerivationBuildInfo :: Cabal.BuildInfo -> Nix.BuildInfo
    convertComponentDerivationBuildInfo = convertBuildInfo $ const True

    convertBuildInfo :: (PackageName -> Bool) -> Cabal.BuildInfo -> Nix.BuildInfo
    convertBuildInfo _ Cabal.BuildInfo {..} | not buildable = mempty
    convertBuildInfo buildDependsFilterPredicate Cabal.BuildInfo {..} = mempty
      & haskell .~ Set.fromList [ resolveInHackage (toNixName x) | (Dependency x _ _) <- targetBuildDepends, buildDependsFilterPredicate x ]
      & system .~ Set.fromList [ resolveInNixpkgs y | x <- extraLibs, y <- libNixName x ]
      & pkgconfig .~ Set.fromList [ resolveInNixpkgs y | PkgconfigDependency x _ <- pkgconfigDepends, y <- libNixName (unPkgconfigName x) ]
      & tool .~ Set.fromList (map resolveInHackageThenNixpkgs . concatMap buildToolNixName
              $ [ unPackageName x | ExeDependency x _ _ <- buildToolDepends ] ++ [ x | LegacyExeDependency x _ <- buildTools ])

    convertSetupBuildInfo :: Cabal.SetupBuildInfo -> Nix.BuildInfo
    convertSetupBuildInfo bi = mempty
      & haskell .~ Set.fromList [ resolveInHackage (toNixName x) | (Dependency x _ _) <- Cabal.setupDepends bi ]

bindNull :: Identifier -> Binding
bindNull i = binding # (i, path # ["null"])

stripRedundanceSpaces :: String -> String
stripRedundanceSpaces = unwords . words
