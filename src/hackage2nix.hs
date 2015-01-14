-- Run: cabal build -j hackage2nix && time dist/build/hackage2nix/hackage2nix >hackage-packages.nix && mv hackage-packages.nix ~/.nix-defexpr/pkgs/development/haskell-modules/hackage-packages.nix && nix-env -qaP -A haskellngPackages | tail -1

module Main ( main ) where

import Cabal2Nix.Flags ( configureCabalFlags )
import Cabal2Nix.Generate ( cabal2nix' )
import Cabal2Nix.Hackage ( readHashedHackage, Hackage )
import Cabal2Nix.Package
import Data.List
import Data.Function
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
import Distribution.NixOS.PackageMap
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
          (missingDeps, _, drv') = cabal2nix resolver descr
          drv = drv' { src = srcSpec, editedCabalFile = if revision drv == 0 then "" else cabalFileHash
                     , metaSection = (metaSection drv') { broken = not (Set.null missing) } -- Missing Haskell dependencies!
                     , jailbreak = not (null missingDeps) -- Dependency constraints aren't fulfilled
                     }

          missing :: Set String
          missing = Set.fromList $ filter (`Set.notMember` knownAttributesSet) (buildDepends drv ++ testDepends drv)

          buildInputs :: Map Attribute (Maybe Path)
          buildInputs = Map.unions
                        [ Map.fromList [ (n, Nothing) | n <- Set.toList missing ]
                        , Map.fromList [ (n, resolveNixpkgsAttribute nixpkgs n) | n <- Set.toAscList (Set.fromList (extraLibs drv ++ pkgConfDeps drv)) ]
                        , Map.fromList [ (n, resolveNixpkgsOrHackageAttribute nixpkgs hackage n) | n <- Set.toAscList (Set.fromList (buildTools drv)) ]
                        ]

          systemOverrides :: Doc
          systemOverrides = fsep (map (uncurry formatOverride) (Map.toAscList buildInputs))

          formatOverride :: Attribute -> Maybe Path -> Doc
          formatOverride n Nothing   = space <> text n <> text " = null;"       -- missing attribute
          formatOverride n (Just [])                                            -- Haskell package:
            | n == name              = formatOverride n Nothing                 --     refers to a missing system library
            | otherwise              = empty                                    --     found by callPackage
          formatOverride n (Just p)  = (text " inherit" <+> parens (text (intercalate "." p)) <+> text n) <> semi

          overrides :: Doc
          overrides = systemOverrides

          attr | Just v <- Map.lookup name generatedDefaultPackageSet, v == version = name
               | otherwise                                                          = name ++ '_' : [ if c == '.' then '_' else c | c <- display version ]

      return $ nest 2 $ hang (string attr <+> equals <+> text "callPackage") 2 (parens (disp drv)) <+> (braces overrides <> semi)
    return (intercalate "\n\n" (map render defs))

  liftIO $ mapM_ (\pkg -> putStrLn pkg >> putStrLn "") pkgs
  liftIO $ putStrLn "}"

resolveNixpkgsOrHackageAttribute :: Nixpkgs -> Hackage -> Attribute -> Maybe Path
resolveNixpkgsOrHackageAttribute nixpkgs hackage name
  | p@(Just _) <- resolveNixpkgsAttribute nixpkgs name   = p
  | Just _ <- Map.lookup name hackage                   = Just []
  | otherwise                                           = Nothing

resolveNixpkgsAttribute :: Nixpkgs -> Attribute -> Maybe Path
resolveNixpkgsAttribute nixpkgs name
  | Just paths <- Map.lookup name nixpkgs = getShortestPath (Set.toList (paths `Set.intersection` goodScopes))
  | otherwise                             = Nothing
  where
    goodScopes :: Set Path
    goodScopes = Set.fromList [[], ["xlibs"], ["gnome"], ["gnome3"], ["kde4"]]

    getShortestPath :: [Path] -> Maybe Path
    getShortestPath [] = Nothing
    getShortestPath ps = Just ("pkgs" : minimumBy (on compare length) ps)

-- These packages replace the latest respective version during dependency resolution.
defaultPackageOverrides :: [Constraint]
defaultPackageOverrides = map (\s -> fromMaybe (error (show s ++ " is not a valid override selector")) (simpleParse s))
  [ "mtl == 2.1.*"                      -- newer versions require transformers > 4, which we cannot provide in GHC 7.8.x
  , "HStringTemplate < 0.8"             -- 0.8 depends on time >= 1.5, which we cannot provide in GHC 7.8.x
  ]

-- These packages are added to the generated set, but the play no role during dependency resolution.
extraPackages :: [Constraint]
extraPackages =
  map (\s -> fromMaybe (error (show s ++ " is not a valid extra package selector")) (simpleParse s))
  [ "Cabal == 1.20.*"                   -- required for cabal-install et al on old GHC versions
  , "Cabal == 1.18.*"
  , "optparse-applicative == 0.10.*"    -- required for elm-make 0.1.1 and elm-package 0.4
  ]

cabal2nix :: (Dependency -> Bool) -> GenericPackageDescription -> ([Dependency], FlagAssignment, Derivation)
cabal2nix resolver cabal = (missingDeps, flags, drv)
  where
    drv = (cabal2nix' descr) -- { configureFlags = [ "-f" ++ (if b then "" else "-") ++ n | (FlagName n, b) <- flags ] }

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
