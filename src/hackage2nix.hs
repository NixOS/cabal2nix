-- Run: cabal build -j hackage2nix && dist/build/hackage2nix/hackage2nix >hackage-packages.nix && mv hackage-packages.nix ~/.nix-defexpr/pkgs/development/haskell-modules/hackage-packages.nix && nix-env -qaP | tail -1

module Main ( main ) where

import Cabal2Nix.Flags ( configureCabalFlags )
import Cabal2Nix.Generate ( cabal2nix' )
import Cabal2Nix.Hackage ( readHashedHackage, Hackage )
import Cabal2Nix.Package
import Data.List
import Control.Monad
import Control.Monad.Par.Combinator
import Control.Monad.Par.IO
import Control.Monad.Trans ( liftIO )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid
import Data.Set ( Set )
import qualified Data.Set as Set
import Distribution.Compiler
import Distribution.NixOS.Derivation.Cabal
import Distribution.NixOS.PackageMap ( PackageMap, readNixpkgPackageMap )
import Distribution.NixOS.PrettyPrinting hiding ( attr, (<>) )
import Distribution.Package
import Distribution.PackageDescription hiding ( buildDepends, extraLibs, buildTools )
import Distribution.PackageDescription.Configuration
import Distribution.System
import Distribution.Text
import Distribution.Version

type Nixpkgs = PackageMap       -- Map String (Set [String])
type PackageSet = Map String Version
type PackageMultiSet = Map String (Set Version)

type Constraint = Dependency

resolveConstraint :: Constraint -> Hackage -> Version
resolveConstraint c = fromMaybe (error ("constraint " ++ display c ++ " cannot be resolved in Hackage")) .
                        resolveConstraint' c

resolveConstraint' :: Constraint -> Hackage -> Maybe Version
resolveConstraint' (Dependency (PackageName name) vrange) hackage | Set.null vs = Nothing
                                                                  | otherwise   = Just (Set.findMax vs)
  where vs = Set.filter (`withinRange` vrange) (Map.keysSet (hackage Map.! name))

main :: IO ()
main = do
  hackage <- readHashedHackage
  nixpkgs <- readNixpkgPackageMap       -- TODO: nix barfs at the generated "type" attribute for some reason.
  runParIO (generatePackageSet (Map.delete "type" hackage) nixpkgs)

generatePackageSet :: Hackage -> Nixpkgs -> ParIO ()
generatePackageSet hackage nixpkgs = do
  let
    corePackageSet :: PackageSet
    corePackageSet = Map.fromList [ (name, v) | PackageIdentifier (PackageName name) v <- corePackages ++ hardCorePackages ]

    latestVersionSet :: PackageSet
    latestVersionSet = Map.map (Set.findMax . Map.keysSet) hackage

    defaultPackageOverridesSet :: PackageSet
    defaultPackageOverridesSet = Map.fromList [ (name, resolveConstraint c hackage) | c@(Dependency (PackageName name) _) <- defaultPackageOverrides ]

    generatedDefaultPackageSet :: PackageSet
    generatedDefaultPackageSet = (defaultPackageOverridesSet `Map.union` latestVersionSet) `Map.difference` corePackageSet

    latestCorePackageSet :: PackageSet
    latestCorePackageSet = latestVersionSet `Map.intersection` corePackageSet

    latestOverridePackageSet :: PackageSet
    latestOverridePackageSet = latestVersionSet `Map.intersection` defaultPackageOverridesSet

    extraPackageSet :: PackageMultiSet
    extraPackageSet = Map.unionsWith Set.union
                        [ Map.singleton name (Set.singleton (resolveConstraint c hackage)) | c@(Dependency (PackageName name) _) <- extraPackages ]

    db :: PackageMultiSet
    db = Map.unionsWith Set.union [ Map.map Set.singleton generatedDefaultPackageSet
                                  , Map.map Set.singleton latestCorePackageSet
                                  , Map.map Set.singleton latestOverridePackageSet
                                  , extraPackageSet
                                  ]

    knownAttributesSet :: Set String
    knownAttributesSet = Set.union (Map.keysSet hackage) (Map.keysSet corePackageSet)

    resolver :: Dependency -> Bool
    resolver (Dependency (PackageName name) vrange)
      | Just v <- Map.lookup name corePackageSet                = v `withinRange` vrange
      | Just v <- Map.lookup name generatedDefaultPackageSet    = v `withinRange` vrange
      | otherwise                                               = False

  liftIO $ do putStrLn "/* hackage-packages.nix is an auto-generated file -- DO NOT EDIT! */"
              putStrLn ""
              putStrLn "{ pkgs, stdenv, callPackage }:"
              putStrLn ""
              putStrLn "self: {"
              putStrLn ""
  pkgs <- flip parMapM (Map.toAscList db) $ \(name, vs) -> do
    defs <- forM (Set.toAscList vs) $ \version -> do
      srcSpec <- liftIO $ sourceFromHackage Nothing (name ++ "-" ++ display version)
      let Just cabalFileHash = lookup "x-cabal-file-hash" (customFieldsPD (packageDescription descr))

          -- TODO: Include list of broken dependencies in the generated output.
          descr = hackage Map.! name Map.! version
          (_, _, drv') = cabal2nix resolver descr
          drv = drv' { src = srcSpec, editedCabalFile = if revision drv == 0 then "" else cabalFileHash }

          missing :: Set String
          missing = Set.fromList $
                      filter (not . isKnownNixpkgAttribute nixpkgs hackage) (extraLibs drv ++ pkgConfDeps drv ++ buildTools drv) ++
                      filter (flip Set.notMember knownAttributesSet) (buildDepends drv ++ testDepends drv)

          missingOverrides :: Doc
          missingOverrides | Set.null missing = empty
                           | otherwise        = fcat [ text (' ':dep++" = null;") | dep <- Set.toAscList missing ] <> space

          conflicts :: Set String
          conflicts = Set.intersection knownAttributesSet (Set.fromList (extraLibs drv ++ pkgConfDeps drv))

          conflictOverrides :: Doc
          conflictOverrides | Set.null conflicts = empty
                            | otherwise          = text " inherit (pkgs) " <> hsep (map text (Set.toAscList conflicts)) <> text "; "

          overrides :: Doc
          overrides = conflictOverrides $+$ missingOverrides

          attr | Just v <- Map.lookup name generatedDefaultPackageSet, v == version = name
               | otherwise                                                          = name ++ '_' : [ if c == '.' then '_' else c | c <- display version ]

      return $ nest 2 $ hang (string attr <+> equals <+> text "callPackage") 2 (parens (disp drv)) <+> (braces overrides <> semi)
    return (intercalate "\n\n" (map render defs))

  liftIO $ mapM_ (\pkg -> putStrLn pkg >> putStrLn "") pkgs
  liftIO $ putStrLn "}"

isKnownNixpkgAttribute :: Nixpkgs -> Hackage -> String -> Bool
isKnownNixpkgAttribute nixpkgs hackage name
  | Just _ <- Map.lookup name hackage   = True
  | otherwise                           = maybe False goodScope (Map.lookup name nixpkgs)
  where
    goodScope :: Set [String] -> Bool
    goodScope = not . Set.null . Set.intersection (Set.fromList [[], ["xlibs"], ["gnome"]])

-- These packages replace the latest respective version during dependency resolution.
defaultPackageOverrides :: [Constraint]
defaultPackageOverrides = map (\s -> fromMaybe (error (show s ++ " is not a valid override selector")) (simpleParse s))
  [ "mtl == 2.1.*"
  , "monad-control == 0.3.*"
  ]

-- These packages are added to the generated set, but the play no role during dependency resolution.
extraPackages :: [Constraint]
extraPackages =
  map (\s -> fromMaybe (error (show s ++ " is not a valid extra package selector")) (simpleParse s))
  [ "Cabal < 1.22"
  ]

cabal2nix :: (Dependency -> Bool) -> GenericPackageDescription -> ([Dependency], FlagAssignment, Derivation)
cabal2nix resolver cabal = (missingDeps, flags, drv)
  where
    drv = (cabal2nix' descr) { configureFlags = [ "-f" ++ (if b then "" else "-") ++ n | (FlagName n, b) <- flags ] }

    Right (descr, flags) = finalize (if null missingDeps then resolver else const True) cabal

    missingDeps :: [Dependency]
    missingDeps = either id (const [])  (finalize resolver cabal')

    finalize :: (Dependency -> Bool) -> GenericPackageDescription -> Either [Dependency] (PackageDescription, FlagAssignment)
    finalize resolver' = finalizePackageDescription
                           (configureCabalFlags (package (packageDescription cabal)))
                           resolver'
                           (Platform X86_64 Linux)                 -- shouldn't be hardcoded
                           (CompilerId GHC (Version [7,8,4] []))   -- ditto
                           []                                      -- no additional constraints

    -- A variant of the cabal file that has all test suites enabled to ensure
    -- that their dependencies are recognized by finalizePackageDescription.
    cabal' :: GenericPackageDescription
    cabal' = cabal { condTestSuites = flaggedTests }

    flaggedTests :: [(String, CondTree ConfVar [Dependency] TestSuite)]
    flaggedTests = map (\(n, t) -> (n, mapTreeData enableTest t)) (condTestSuites cabal)

    enableTest :: TestSuite -> TestSuite
    enableTest t = t { testEnabled = True }

corePackages :: [PackageIdentifier]             -- Core packages found on Hackageg
corePackages = map (\s -> fromMaybe (error (show s ++ " is not a valid core package")) (simpleParse s))
  [ "Cabal-1.18.1.5"
  , "array-0.5.0.0"
  , "base-4.7.0.2"
  , "binary-0.7.1.0"
  , "bytestring-0.10.4.0"
  , "containers-0.5.5.1"
  , "deepseq-1.3.0.2"
  , "directory-1.2.1.0"
  , "filepath-1.3.0.2"
  , "ghc-prim-0.3.1.0"
  , "haskeline-0.7.1.2"
  , "haskell2010-1.1.2.0"
  , "haskell98-2.0.0.3"
  , "hoopl-3.10.0.1"
  , "hpc-0.6.0.1"
  , "integer-gmp-0.5.1.0"
  , "old-locale-1.0.0.6"
  , "old-time-1.1.0.2"
  , "pretty-1.1.1.1"
  , "process-1.2.0.0"
  , "template-haskell-2.9.0.0"
  , "terminfo-0.4.0.0"
  , "time-1.4.2"
  , "transformers-0.3.0.0"
  , "unix-2.7.0.1"
  , "xhtml-3000.2.1"
  ]

hardCorePackages :: [PackageIdentifier]         -- Core packages not found on Hackage.
hardCorePackages = map (\s -> fromMaybe (error (show s ++ " is not a valid core package")) (simpleParse s))
  [ "bin-package-db-0.0.0.0"
  , "ghc-7.8.4"
  , "rts-1.0"
  ]
