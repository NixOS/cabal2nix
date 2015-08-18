{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Distribution.Nixpkgs.Haskell.Generate
  ( ResolveM(), TestStatus(..), resolve, resolveTryJailbreak, disableVersionCheck, disableDependencyCheck, succeed, mapSuccess, captureSuccess
  , Nixpkgs, PackageSet, PackageMultiSet
  , PackageSetConfig(..), resolvePackageSet, writePackageSet
  , ResolvedPackageSet(..), resolvedPackages, resolvedDefaultVersions
  ) where

import Cabal2Nix.Generate ( cabal2nix' )
import Cabal2Nix.Package
import Control.Lens
import Control.Monad
import Control.Monad.Par.Combinator ( parMapM )
import Control.Monad.Par.IO ( ParIO )
import Control.Monad.Reader
import Control.Monad.Except
import Data.Ord
import Data.Function
import Data.List
import Data.List.Split (chunksOf)
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Set ( Set )
import qualified Data.Set as Set
import Distribution.Compiler
import Distribution.Nixpkgs.Haskell
import Distribution.Nixpkgs.Meta
import Distribution.Nixpkgs.PackageMap
import Distribution.Nixpkgs.Util.PrettyPrinting hiding ( attr, (<>) )
import Distribution.Package
import Distribution.PackageDescription hiding ( options, buildDepends, extraLibs, buildTools )
import Distribution.PackageDescription.Configuration
import Distribution.System
import Distribution.Text
import Distribution.Version
import Language.Nix.Identifier

newtype ResolveM a = ResolveM
  { unResolveM :: ReaderT (PackageSetConfig, Dependency -> Bool) (ExceptT Derivation IO) a
  } deriving (Functor, Applicative, Monad, MonadIO)

succeed :: Derivation -> ResolveM a
succeed = ResolveM . throwError

captureSuccess :: ResolveM a -> ResolveM (Either Derivation a)
captureSuccess (ResolveM x) = ResolveM $ fmap Right x `catchError` (return . Left)

disableVersionCheck :: ResolveM a -> ResolveM a
disableVersionCheck (ResolveM m) = mapSuccess (jailbreak .~ True) $
    ResolveM $ local (over (_2.argument) ignoreVersion) m
  where ignoreVersion (Dependency name _) = Dependency name anyVersion

disableDependencyCheck :: ResolveM a -> ResolveM a
disableDependencyCheck (ResolveM m) = mapSuccess (metaSection.broken .~ True) $
  ResolveM $ local (_2 .~ const True) m

configureEnableTests :: GenericPackageDescription -> GenericPackageDescription
configureEnableTests pkg = pkg { condTestSuites = map (over _2 enableAlways) (condTestSuites pkg) }
  where enableAlways = mapTreeData (\t -> t { testEnabled = True })

data TestStatus = EnableTests | DisableTests deriving (Show, Read, Eq, Ord)

resolve :: TestStatus -> FlagAssignment -> [Dependency] -> GenericPackageDescription -> ResolveM [Dependency]
resolve test explicitFlags extraConstraints cabal = join . ResolveM $ reader $ \(config, resolver) ->
    either return (succeed . derive . fst) $ finalize config resolver
  where
    configure = if test == EnableTests then configureEnableTests else id
    derive descr = setTests . setFlags . setSource . cabal2nix' $ descr
      where
        setTests = doCheck .~ (test == EnableTests)
        setFlags = cabalFlags .~ explicitFlags
        setSource drv = drv & src .~ sourceFromHackageHash sha256 (display $ packageId descr)
        sha256 | Just x <- lookup "X-Package-SHA256" (customFieldsPD descr) = x
               | otherwise = error $ "resolve: " ++ display (packageId descr) ++ " has no hash"

    finalize :: PackageSetConfig -> (Dependency -> Bool) -> Either [Dependency] (PackageDescription, FlagAssignment)
    finalize config resolver' = finalizePackageDescription
      explicitFlags resolver'                 -- flags / information about available packages
      (platform config) (compilerInfo config) -- system information. FIXME: nix expressions should work on multiple systems
      extraConstraints                        -- additional constraints
      (configure cabal)                       -- configured (with test status set) package description

resolveTryJailbreak :: TestStatus -> FlagAssignment -> [Dependency] -> GenericPackageDescription -> ResolveM [Dependency]
resolveTryJailbreak test explicitFlags extraConstraints cabal = do
  void $ resolve test explicitFlags extraConstraints cabal
  disableVersionCheck $
    resolve test explicitFlags extraConstraints cabal

mapSuccess :: (Derivation -> Derivation) -> ResolveM a -> ResolveM a
mapSuccess f (ResolveM (ReaderT m)) = ResolveM . ReaderT $ mapExceptT (over (mapped._Left) f) . m
--------------------------------------------------------------------------------

type Nixpkgs = PackageMap       -- Map String (Set [String])
type PackageSet = Map String (Version, ResolveM [Dependency])
type PackageMultiSet = Map String [(Version, ResolveM [Dependency])]

data PackageSetConfig = PackageSetConfig
  {
  -- |Target architecture. Used by 'finalizePackageDescription' to
  -- choose appropriate flags and dependencies.
    platform :: Platform

  -- |Target compiler. Used by 'finalizePackageDescription' to choose
  -- appropriate flags and dependencies.
  , compilerInfo :: CompilerInfo

  -- |These packages are assumed to be available for dependency resolution,
  -- but no entry is generated for them. For example, libraries that are bundled
  -- with GHC are core packages.
  , corePackages :: [PackageIdentifier]

  -- |These packages are used for dependency resolution. Only one version
  -- per package is allowed here.
  , defaultPackages :: PackageSet

  -- |These extra packages are added to the package set but they play no
  -- role during dependency resolution. There may be multiple additional
  -- versions for a package.
  , extraPackages :: PackageMultiSet
  }

data ResolvedPackageSet = ResolvedPackageSet
  { _resolvedDefaultVersions :: Map String Version
  , _resolvedPackages :: [(String, IO (Either [Dependency] Derivation))]
  }
makeLenses ''ResolvedPackageSet

resolvePackageSet :: PackageSetConfig -> ResolvedPackageSet
resolvePackageSet config = ResolvedPackageSet resolverPackageSet $ do
    (name, pkgs) <- Map.toList packages
    (withVersion, pkg) <- sortBy (comparing $ fst . snd) pkgs
    return $ makePackageEntry withVersion name pkg
  where
    corePackageSet = Map.fromList [ (name, v) | PackageIdentifier (PackageName name) v <- corePackages config ]
    resolverPackageSet = corePackageSet `Map.union` Map.map fst (defaultPackages config)
    resolver (Dependency (PackageName name) vrange) = maybe False (`withinRange` vrange) $ Map.lookup name resolverPackageSet
    runResolveM = fmap (view swapped) . runExceptT . flip runReaderT (config, resolver) . unResolveM

    makePackageEntry :: Bool -> String -> (Version, ResolveM [Dependency]) -> (String, IO (Either [Dependency] Derivation))
    makePackageEntry withVersion name (pkgversion, pkg) = (name ++ suffix, runResolveM pkg)
      where
        suffix = if withVersion || name `Map.member` corePackageSet then versionSuffix else ""
        versionSuffix = '_' : [ if c == '.' then '_' else c | c <- display pkgversion ]

    packages = Map.unionsWith (++)
      [ Map.map (\x -> [(False, x)]) $ defaultPackages config
      , Map.map (map $ (,) True) $ extraPackages config
      ]

writePackageSet :: Nixpkgs -> ResolvedPackageSet -> ParIO ()
writePackageSet nixpkgs (ResolvedPackageSet defaultVersions packages)  = do
  liftIO $ do putStrLn "/* This is an auto-generated file -- DO NOT EDIT! */"
              putStrLn ""
              putStrLn "{ pkgs, stdenv, callPackage }:"
              putStrLn ""
              putStrLn "self: {"
              putStrLn ""
  forM_ (chunksOf 100 packages) $ \batch -> do
    pkgs <- flip parMapM batch $ \(attr, resolvePkg) -> do
      let failMissing missing =
            fail $ "*** derivation " ++ attr ++ " is missing dependencies: " ++ unwords (map display missing)
      drv' <- liftIO $ either failMissing return =<< resolvePkg

      let
        haskellDependencies :: Set String
        haskellDependencies = Set.map (view ident) $ mconcat [ drv'^.x.haskell | x <- [libraryDepends,executableDepends,testDepends] ]

        systemDependencies :: Set String
        systemDependencies = Set.map (view ident) $ mconcat [ drv'^.x.y | x <- [libraryDepends,executableDepends,testDepends], y <- [system,pkgconfig] ]

        haskellOrSystemDependencies :: Set String
        haskellOrSystemDependencies = Set.map (view ident) $ mconcat [ drv'^.x.tool | x <- [libraryDepends,executableDepends,testDepends] ]

        missingHaskell :: Set String
        missingHaskell = Set.filter (`Map.notMember` defaultVersions) haskellDependencies

        buildInputs :: Map Attribute (Maybe Path)
        buildInputs = Map.unions
                      [ Map.fromList [ (n, Nothing) | n <- Set.toList missingHaskell ]
                      , Map.fromList [ (n, resolveNixpkgsAttribute nixpkgs n) | n <- Set.toAscList systemDependencies ]
                      , Map.fromList [ (n, resolveHaskellThenNixpkgsAttribute nixpkgs defaultVersions n) | n <- Set.toAscList haskellOrSystemDependencies ]
                      ]

        overrides :: Doc
        overrides = fsep (map (uncurry formatOverride) (Map.toAscList buildInputs))

        formatOverride :: Attribute -> Maybe Path -> Doc
        formatOverride n Nothing   = space <> text n <> text " = null;"       -- missing attribute
        formatOverride _ (Just ["self"]) = mempty -- callPackage finds this package already
        formatOverride n (Just []) = (text " inherit" <+> text n) <> semi
        formatOverride n (Just p)  = (text " inherit" <+> parens (text (intercalate "." p)) <+> text n) <> semi

        drv = drv' & (if attr == "jailbreak-cabal" then jailbreak .~ False else id)

      return $ render $ nest 2 $ hang (string attr <+> equals <+> text "callPackage") 2 (parens (pPrint drv)) <+> (braces overrides <> semi)
    liftIO $ mapM_ (\pkg -> putStrLn pkg >> putStrLn "") pkgs
  liftIO $ putStrLn "}"

resolveHaskellThenNixpkgsAttribute :: Nixpkgs -> Map String Version -> Attribute -> Maybe Path
resolveHaskellThenNixpkgsAttribute nixpkgs haskellPackages name
  | name `Map.member` haskellPackages                  = Just ["self"]
  | p@(Just _) <- resolveNixpkgsAttribute nixpkgs name = p
  | otherwise                                          = Nothing

resolveNixpkgsAttribute :: Nixpkgs -> Attribute -> Maybe Path
resolveNixpkgsAttribute nixpkgs name
  | name `elem` ["clang","lldb","llvm"]   = Just ["self","llvmPackages"]
  | Just paths <- Map.lookup name nixpkgs = getShortestPath (Set.toList (paths `Set.intersection` goodScopes))
  | otherwise                             = Nothing
  where
    goodScopes :: Set Path
    goodScopes = Set.fromList [[], ["xlibs"], ["gnome"], ["gnome3"], ["kde4"]]

    getShortestPath :: [Path] -> Maybe Path
    getShortestPath [] = Nothing
    getShortestPath ps = Just ("pkgs" : minimumBy (on compare length) ps)
