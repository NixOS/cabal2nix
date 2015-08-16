{-# LANGUAGE TemplateHaskell #-}
module Distribution.Nixpkgs.Generate 
  ( Configuration(..), Nixpkgs, PackageSet, PackageMultiSet
  , ResolvedPackage(..), resolvedMissing, resolvedFlags, resolvedDerivation
  , resolvePackageSet
  , writePackageSet
  ) where

import Cabal2Nix.Generate ( cabal2nix' )
import Cabal2Nix.HackageGit ( Hackage )
import Cabal2Nix.Package    
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Par.Class ( spawnP, get )
import Control.Monad.Par.IO ( ParIO )
import Control.Monad.Trans ( liftIO )
import Data.Ord    
import Data.Function
import Data.List
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

type Nixpkgs = PackageMap       -- Map String (Set [String])
type PackageSet = Map String (FlagAssignment, GenericPackageDescription)
type PackageMultiSet = Map String [(FlagAssignment, GenericPackageDescription)]

data Configuration = Configuration
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
  deriving (Show)

data ResolvedPackage = ResolvedPackage
  { _resolvedMissing :: [Dependency]
  , _resolvedFlags :: FlagAssignment
  , _resolvedDerivation :: Derivation
  }
makeLenses ''ResolvedPackage

resolvePackageSet :: Configuration -> [(String, ResolvedPackage)]
resolvePackageSet config = concat $ Map.elems packages 
  where
    corePackageSet = Map.fromList [ (name, v) | PackageIdentifier (PackageName name) v <- corePackages config ]
    lookupPackageVersion name = Map.lookup name corePackageSet <|> 
      pkgVersion . package . packageDescription . snd <$> Map.lookup name (defaultPackages config)
    resolver (Dependency (PackageName name) vrange) = maybe False (`withinRange` vrange) $ lookupPackageVersion name  

    makePackageEntry :: Bool -> String -> (FlagAssignment, GenericPackageDescription) -> (String, ResolvedPackage)
    makePackageEntry withVersion name pkg = (name ++ suffix, setSource $ cabal2nix config resolver pkg)
      where 
        suffix = if withVersion || name `Map.member` corePackageSet then versionSuffix else ""
        versionSuffix = '_' : [ if c == '.' then '_' else c | c <- display pkgversion ]

        descr = packageDescription $ snd pkg
        pkgversion = pkgVersion $ package descr 

        setSource drv = drv & resolvedDerivation.src .~ sourceFromHackageHash sha256 (name ++ "-" ++ display pkgversion)
        sha256 | Just x <- lookup "X-Package-SHA256" (customFieldsPD descr) = x
               | otherwise = error $ display (packageId descr) ++ " has no hash" 

    defaultPackagesGenerated = Map.mapWithKey (\name pkg -> [makePackageEntry False name pkg]) $ defaultPackages config
    extraPackagesGenerated = Map.mapWithKey (\name pkgs -> map (makePackageEntry True name) pkgs) $ extraPackages config
    sortVersions = sortBy . comparing . view $ _2.resolvedDerivation.pkgid.to pkgVersion
    packages = Map.map sortVersions $ Map.unionWith (++) extraPackagesGenerated defaultPackagesGenerated 

cabal2nix :: Configuration -> (Dependency -> Bool) -> (FlagAssignment, GenericPackageDescription) -> ResolvedPackage 
cabal2nix config resolver (explicitFlags, cabal) = either id makeBrokenPackage $ do
    void $ resolvePackageWith True False
    void $ resolvePackageWith True True
    void $ resolvePackageWith False False
    resolvePackageWith False True
  where
    cabalWithTests = cabal { condTestSuites = enabledTestSuites }
    cabalWithoutTests = cabal { condTestSuites = [] }

    enabledTestSuites = map (over _2 $ mapTreeData enableTest) (condTestSuites cabal)
    enableTest t = t { testEnabled = True }
    resolverJailbreak (Dependency pkg _) = resolver $ Dependency pkg anyVersion
  
    resolvePackageWith :: Bool -> Bool -> Either ResolvedPackage [Dependency] 
    resolvePackageWith check jail = view swapped $ do
      let cabal' = if check then cabalWithTests else cabalWithoutTests
          resolver' = if jail then resolverJailbreak else resolver
      (descr, flags) <- finalize resolver' cabal'
      return $ ResolvedPackage [] flags $ cabal2nix' descr & doCheck .~ check & jailbreak .~ jail

    finalize :: (Dependency -> Bool) -> GenericPackageDescription -> Either [Dependency] (PackageDescription, FlagAssignment)
    finalize resolver' = finalizePackageDescription explicitFlags resolver' (platform config) (compilerInfo config) []

    Right (brokenDescr, brokenFlags) = finalize (const True) cabal
    makeBrokenPackage missing = ResolvedPackage missing brokenFlags $ cabal2nix' brokenDescr & jailbreak .~ True
    
writePackageSet :: Nixpkgs -> Hackage -> [(String, ResolvedPackage)] -> ParIO ()
writePackageSet nixpkgs hackage packageSet = do
  liftIO $ do putStrLn "/* This is an auto-generated file -- DO NOT EDIT! */"
              putStrLn ""
              putStrLn "{ pkgs, stdenv, callPackage }:"
              putStrLn ""
              putStrLn "self: {"
              putStrLn ""
  pkgs <- forM packageSet $ \(attr, (ResolvedPackage missingDeps _ drv')) -> do
    let 
        haskellDependencies :: Set String
        haskellDependencies = Set.map (view ident) $ mconcat [ drv'^.x.haskell | x <- [libraryDepends,executableDepends,testDepends] ]

        systemDependencies :: Set String
        systemDependencies = Set.map (view ident) $ mconcat [ drv'^.x.y | x <- [libraryDepends,executableDepends,testDepends], y <- [system,pkgconfig] ]

        haskellOrSystemDependencies :: Set String
        haskellOrSystemDependencies = Set.map (view ident) $ mconcat [ drv'^.x.tool | x <- [libraryDepends,executableDepends,testDepends] ]

        missingHaskell :: Set String
        missingHaskell = Set.fromList [ name | Dependency (PackageName name) _ <- missingDeps ] `Set.intersection` haskellDependencies

        buildInputs :: Map Attribute (Maybe Path)
        buildInputs = Map.unions
                      [ Map.fromList [ (n, Nothing) | n <- Set.toList missingHaskell ]
                      , Map.fromList [ (n, resolveNixpkgsAttribute nixpkgs n) | n <- Set.toAscList systemDependencies ]
                      , Map.fromList [ (n, resolveHackageThenNixpkgsAttribute nixpkgs hackage n) | n <- Set.toAscList haskellOrSystemDependencies ]
                      ]

        overrides :: Doc
        overrides = fsep (map (uncurry formatOverride) (Map.toAscList buildInputs))

        formatOverride :: Attribute -> Maybe Path -> Doc
        formatOverride n Nothing   = space <> text n <> text " = null;"       -- missing attribute
        formatOverride _ (Just ["self"]) = mempty -- callPackage finds this package already
        formatOverride n (Just []) = (text " inherit" <+> text n) <> semi
        formatOverride n (Just p)  = (text " inherit" <+> parens (text (intercalate "." p)) <+> text n) <> semi

    let drv = drv' & (if attr == "jailbreak-cabal" then jailbreak .~ False else id)
                   & metaSection . broken .~ not (Set.null missingHaskell)                -- Missing Haskell dependencies!

    spawnP $ render $ nest 2 $ hang (string attr <+> equals <+> text "callPackage") 2 (parens (pPrint drv)) <+> (braces overrides <> semi)

  mapM_ (\pkg -> get pkg >>= liftIO . putStrLn >> liftIO (putStrLn "")) pkgs
  liftIO $ putStrLn "}"

resolveHackageThenNixpkgsAttribute :: Nixpkgs -> Hackage -> Attribute -> Maybe Path
resolveHackageThenNixpkgsAttribute nixpkgs hackage name
  | Just _ <- Map.lookup name hackage                   = Just ["self"]
  | p@(Just _) <- resolveNixpkgsAttribute nixpkgs name  = p
  | otherwise                                           = Nothing

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
