{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main ( main ) where

import Control.Lens
import Control.Monad
import Control.Monad.Par.Combinator
import Control.Monad.Par.IO
import Control.Monad.Trans ( liftIO )
import Data.List
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.String
import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.Haskell
import Distribution.Nixpkgs.Haskell.Constraint
import Distribution.Nixpkgs.Haskell.FromCabal ( fromGenericPackageDescription )
import Distribution.Nixpkgs.Haskell.FromCabal.Configuration
import Distribution.Nixpkgs.Haskell.FromCabal.Flags
import Distribution.Nixpkgs.Haskell.PackageSourceSpec
import Distribution.Nixpkgs.Meta
import Distribution.Nixpkgs.PackageMap
import Distribution.Package
import Distribution.PackageDescription hiding ( options, buildDepends, extraLibs, buildTools )
import Distribution.System
import Distribution.Text
import Distribution.Version
import HackageGit ( readHackage, Hackage )
import Language.Nix
import Options.Applicative
import Paths_hackage2nix
import System.FilePath
import Text.PrettyPrint.HughesPJClass hiding ( (<>) )

type Nixpkgs = PackageMap       -- Map String (Set [String])
type PackageSet = Map String Version
type PackageMultiSet = Map String (Set Version)

resolveConstraint :: Constraint -> Hackage -> Version
resolveConstraint c = fromMaybe (error msg) . resolveConstraint' c
  where msg = "constraint " ++ display c ++ " cannot be resolved in Hackage"

resolveConstraint' :: Constraint -> Hackage -> Maybe Version
resolveConstraint' (Dependency (PackageName name) vrange) hackage
  | Just vset' <- Map.lookup name hackage
  , vset <- Set.filter (`withinRange` vrange) (Map.keysSet vset')
  , not (Set.null vset)         = Just (Set.findMax vset)
  | otherwise                   = Nothing

data Options = Options
  { hackageRepository :: FilePath
  , preferredVersionsFile :: Maybe FilePath
  , nixpkgsRepository :: FilePath
  , configFile :: FilePath
  , targetPlatform :: Platform
  }
  deriving (Show)

options :: Parser Options
options = Options
          <$> strOption (long "hackage" <> help "path to Hackage git repository" <> value "hackage" <> showDefaultWith id <> metavar "PATH")
          <*> optional (strOption (long "preferred-versions" <> help "path to Hackage preferred-versions file" <> value "hackage/preferred-versions" <> showDefault <> metavar "PATH"))
          <*> strOption (long "nixpkgs" <> help "path to Nixpkgs repository" <> value "<nixpkgs>" <> showDefaultWith id <> metavar "PATH")
          <*> strOption (long "config" <> help "path to configuration file" <> value "nixpkgs/pkgs/development/haskell-modules/configuration-hackage2nix.yaml" <> showDefaultWith id <> metavar "PATH")
          <*> option (fmap fromString str) (long "platform" <> help "target platform to generate package set for" <> value "x86_64-linux" <> showDefaultWith display <> metavar "PLATFORM")

pinfo :: ParserInfo Options
pinfo = info
        (   helper
        <*> infoOption ("hackage2nix " ++ display version) (long "version" <> help "Show version number")
        <*> options
        )
        (  fullDesc
        <> header "hackage2nix converts a Hackage database into a haskell-packages.nix file."
        )

main :: IO ()
main = do
  Options {..} <- execParser pinfo

  defaultConfiguration <- readConfiguration configFile
  hackage <- readHackage hackageRepository
  nixpkgs <- readNixpkgPackageMap nixpkgsRepository Nothing
  preferredVersions <- readPreferredVersions (fromMaybe (hackageRepository </> "preferred-versions") preferredVersionsFile)
  let fixup = Map.delete "acme-everything"      -- TODO: https://github.com/NixOS/cabal2nix/issues/164
            . Map.delete "som"                  -- TODO: https://github.com/NixOS/cabal2nix/issues/164
            . Map.delete "type"                 -- TODO: https://github.com/NixOS/cabal2nix/issues/163
            . Map.delete "dictionary-sharing"   -- TODO: https://github.com/NixOS/cabal2nix/issues/175
            . Map.filter (/= Map.empty)
            . Map.mapWithKey (enforcePreferredVersions preferredVersions)
  runParIO $ generatePackageSet targetPlatform defaultConfiguration (fixup hackage) nixpkgs

enforcePreferredVersions :: [Constraint] -> String -> Map Version GenericPackageDescription
                         -> Map Version GenericPackageDescription
enforcePreferredVersions cs pkg = Map.filterWithKey (\v _ -> PackageIdentifier (PackageName pkg) v `satisfiesConstraints` cs)

generatePackageSet :: Platform -> Configuration -> Hackage -> Nixpkgs -> ParIO ()
generatePackageSet targetPlatform config hackage nixpkgs = do
  let
    corePackageSet :: PackageSet
    corePackageSet = Map.fromList [ (name, v) | PackageIdentifier (PackageName name) v <- Set.toList (corePackages config) ]

    latestVersionSet :: PackageSet
    latestVersionSet = Map.map (Set.findMax . Map.keysSet) hackage

    defaultPackageOverridesSet :: PackageSet
    defaultPackageOverridesSet = Map.fromList [ (name, resolveConstraint c hackage) | c@(Dependency (PackageName name) _) <- defaultPackageOverrides config ]

    generatedDefaultPackageSet :: PackageSet
    generatedDefaultPackageSet = (defaultPackageOverridesSet `Map.union` latestVersionSet) `Map.difference` corePackageSet

    latestCorePackageSet :: PackageSet
    latestCorePackageSet = latestVersionSet `Map.intersection` corePackageSet

    latestOverridePackageSet :: PackageSet
    latestOverridePackageSet = latestVersionSet `Map.intersection` defaultPackageOverridesSet

    extraPackageSet :: PackageMultiSet
    extraPackageSet = Map.unionsWith Set.union
                        [ Map.singleton name (Set.singleton (resolveConstraint c hackage)) | c@(Dependency (PackageName name) _) <- extraPackages config ]

    db :: PackageMultiSet
    db = Map.unionsWith Set.union [ Map.map Set.singleton generatedDefaultPackageSet
                                  , Map.map Set.singleton latestCorePackageSet
                                  , Map.map Set.singleton latestOverridePackageSet
                                  , extraPackageSet
                                  ]

    haskellResolver :: Dependency -> Bool
    haskellResolver (Dependency (PackageName name) vrange)
      | Just v <- Map.lookup name corePackageSet                = v `withinRange` vrange
      | Just v <- Map.lookup name generatedDefaultPackageSet    = v `withinRange` vrange
      | otherwise                                               = False

    nixpkgsResolver :: Identifier -> Maybe Binding
    nixpkgsResolver = resolve (Map.map (Set.map (over path ("pkgs":))) nixpkgs)

    globalPackageMaintainers :: Map PackageName (Set Identifier)
    globalPackageMaintainers = Map.unionsWith Set.union [ Map.singleton p (Set.singleton m) | (m,ps) <- Map.toList (packageMaintainers config), p <- Set.toList ps ]

  liftIO $ do putStrLn "/* hackage-packages.nix is an auto-generated file -- DO NOT EDIT! */"
              putStrLn ""
              putStrLn "{ pkgs, stdenv, callPackage }:"
              putStrLn ""
              putStrLn "self: {"
              putStrLn ""
  pkgs <- flip parMapM (Map.toAscList db) $ \(name, vs) -> do
    defs <- forM (Set.toAscList vs) $ \pkgversion -> do
      let -- TODO: Include list of broken dependencies in the generated output.
          descr = hackage Map.! name Map.! pkgversion

          flagAssignment :: FlagAssignment
          flagAssignment = configureCabalFlags (packageId descr)

          drv' :: Derivation
          drv' = fromGenericPackageDescription haskellResolver nixpkgsResolver targetPlatform (compilerInfo config) flagAssignment [] descr

          isInDefaultPackageSet :: Bool
          isInDefaultPackageSet = maybe False (== pkgversion) (Map.lookup name generatedDefaultPackageSet)

          attr :: String
          attr = name ++ if isInDefaultPackageSet then [] else '_' : [ if c == '.' then '_' else c | c <- display pkgversion ]

          sha256 :: String
          sha256 | Just x <- lookup "X-Package-SHA256" (customFieldsPD (packageDescription descr)) = x
                 | otherwise = error $ display (packageId descr) ++ " has no hash"

      srcSpec <- liftIO $ sourceFromHackage (Certain sha256) (name ++ "-" ++ display pkgversion)

      let drv = drv' & src .~ srcSpec
                     & metaSection.hydraPlatforms %~ (`Set.difference` fromMaybe Set.empty (Map.lookup (PackageName name) (dontDistributePackages config)))
                     & metaSection.hydraPlatforms %~ (if isInDefaultPackageSet then id else const Set.empty)
                     & metaSection.maintainers .~ fromMaybe Set.empty (Map.lookup (PackageName name) globalPackageMaintainers)

          isFromHackage :: Binding -> Bool
          isFromHackage b = case view (reference . path) b of
                              ["self",_] -> True
                              _ -> False

          overrides :: Doc
          overrides = fcat $ punctuate space
                        [ pPrint b | b <- Set.toList (view (dependencies . each) drv), not (isFromHackage b) ]

      return $ nest 2 $ hang (doubleQuotes (text  attr) <+> equals <+> text "callPackage") 2 (parens (pPrint drv)) <+> (braces overrides <> semi)
    return (intercalate "\n\n" (map render defs))

  liftIO $ mapM_ (\pkg -> putStrLn pkg >> putStrLn "") pkgs
  liftIO $ putStrLn "}"

readPreferredVersions :: FilePath -> IO [Constraint]
readPreferredVersions p = mapMaybe parsePreferredVersionsLine . lines <$> readFile p

parsePreferredVersionsLine :: String -> Maybe Constraint
parsePreferredVersionsLine [] = Nothing
parsePreferredVersionsLine ('-':'-':_) = Nothing
parsePreferredVersionsLine l = simpleParse l `mplus` error ("invalid preferred-versions line: " ++ show l)



-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RecordWildCards #-}
--
-- module Main ( main ) where
--
-- import Control.Lens
-- -- import Control.Exception
-- import Control.Monad
-- import Control.Monad.Par.Combinator
-- import Control.Monad.Par.IO as ParIO
-- import Control.Monad.Par as Par
-- -- import Control.Monad.Trans ( liftIO )
-- -- import Data.List
-- import Data.Map.Strict ( Map )
-- import qualified Data.Map.Strict as Map
-- import Data.Maybe
-- import Data.Monoid
-- import Data.Set ( Set )
-- import qualified Data.Set as Set
-- import Data.String
-- import Distribution.Compiler
-- import Distribution.Nixpkgs.Fetch
-- import Distribution.Nixpkgs.Haskell
-- -- import Distribution.Nixpkgs.Haskell.Constraint
-- import Distribution.Nixpkgs.Haskell.FromCabal ( fromGenericPackageDescription )
-- -- import Distribution.Nixpkgs.Haskell.FromCabal.Configuration
-- -- import Distribution.Nixpkgs.Haskell.FromCabal.Flags
-- import Distribution.Nixpkgs.Haskell.HackageGit ( readHackage, Hackage )
-- -- import Distribution.Nixpkgs.Haskell.PackageSourceSpec
-- -- import Distribution.Nixpkgs.Meta
-- import Distribution.Nixpkgs.PackageMap
-- import Distribution.Package
-- import Distribution.PackageDescription hiding ( options, buildDepends, extraLibs, buildTools )
-- import Distribution.System
-- import Distribution.Text
-- import Distribution.Version
-- import Language.Nix
-- import Options.Applicative
-- import qualified Paths_hackage2nix as Main
-- import System.FilePath
-- import Text.PrettyPrint.HughesPJClass hiding ( (<>) )
-- import Stackage
-- import System.IO
--
-- -- type Nixpkgs = PackageMap       -- Map String (Set [String])
-- -- type PackageSet = Map PackageName Version
-- -- type PackageMultiSet = Map String (Set Version)
-- --
-- -- resolveConstraint :: Constraint -> Hackage -> Version
-- -- resolveConstraint c = fromMaybe (error msg) . resolveConstraint' c
-- --   where msg = "constraint " ++ display c ++ " cannot be resolved in Hackage"
-- --
-- -- resolveConstraint' :: Constraint -> Hackage -> Maybe Version
-- -- resolveConstraint' (Dependency (PackageName name) vrange) hackage
-- --   | Just vset' <- Map.lookup name hackage
-- --   , vset <- Set.filter (`withinRange` vrange) (Map.keysSet vset')
-- --   , not (Set.null vset)         = Just (Set.findMax vset)
-- --   | otherwise                   = Nothing
--
-- data Options = Options
--   { hackageRepository :: FilePath
--   , preferredVersionsFile :: Maybe FilePath
--   , nixpkgsRepository :: FilePath
--   , ltsHaskellRepository :: FilePath
--   , stackageNightlyRepository :: FilePath
--   , configFile :: FilePath
--   , targetPlatform :: Platform
--   }
--   deriving (Show)
--
-- options :: Parser Options
-- options = Options
--           <$> strOption (long "hackage" <> help "path to Hackage git repository" <> value "hackage" <> showDefaultWith id <> metavar "PATH")
--           <*> optional (strOption (long "preferred-versions" <> help "path to Hackage preferred-versions file" <> value "hackage/preferred-versions" <> showDefault <> metavar "PATH"))
--           <*> strOption (long "nixpkgs" <> help "path to Nixpkgs repository" <> value "/home/simons/src/cabal2nix/hackage2nix/nixpkgs" <> showDefaultWith id <> metavar "PATH")
--           <*> strOption (long "lts-haskell" <> help "path to LTS Haskell repository" <> value "lts-haskell" <> showDefaultWith id <> metavar "PATH")
--           <*> strOption (long "stackage-nightly" <> help "path to Stackage Nightly repository" <> value "stackage-nightly" <> showDefaultWith id <> metavar "PATH")
--           <*> strOption (long "config" <> help "path to configuration file" <> value "nixpkgs/pkgs/development/haskell-modules/configuration-hackage2nix.yaml" <> showDefaultWith id <> metavar "PATH")
--           <*> option (fmap fromString str) (long "platform" <> help "target platform to generate package set for" <> value "x86_64-linux" <> showDefaultWith display <> metavar "PLATFORM")
--
-- pinfo :: ParserInfo Options
-- pinfo = info
--         (   helper
--         <*> infoOption ("hackage2nix " ++ display Main.version) (long "version" <> help "Show version number")
--         <*> options
--         )
--         (  fullDesc
--         <> header "hackage2nix converts a Hackage database into a hackage-packages.nix file."
--         )
--
-- main :: IO ()
-- main = do
--   Options {..} <- execParser pinfo
--
--   -- defaultConfiguration <- readConfiguration configFile
--   nixpkgs <- readNixpkgPackageMap "/home/simons/src/cabal2nix/hackage2nix/nixpkgs" Nothing
--   -- preferredVersions <- readPreferredVersions (fromMaybe (hackageRepository </> "preferred-versions") preferredVersionsFile)
--   let fixup = Map.delete "acme-everything"      -- TODO: https://github.com/NixOS/cabal2nix/issues/164
--             . Map.delete "som"                  -- TODO: https://github.com/NixOS/cabal2nix/issues/164
--             . Map.delete "type"                 -- TODO: https://github.com/NixOS/cabal2nix/issues/163
--             . Map.delete "dictionary-sharing"   -- TODO: https://github.com/NixOS/cabal2nix/issues/175
--   --        . Map.filter (/= Map.empty)
--   --        . Map.mapWithKey (enforcePreferredVersions preferredVersions)
--   hackage <- fixup <$> readHackage hackageRepository
--   snapshots <- ParIO.runParIO (readStackage ltsHaskellRepository)
--
--   let
--       allStackagePackages :: Map PackageName (Set Spec)
--       allStackagePackages = Map.unionsWith Set.union (map (Map.map Set.singleton . packages) snapshots)
--
--       notInStackage :: String -> Map Version GenericPackageDescription -> Bool
--       notInStackage n _ = isNothing (Map.lookup (PackageName n) allStackagePackages)
--
--       nonStackagePackages :: Map String Version
--       nonStackagePackages = Map.map (Set.findMax . Map.keysSet) (Map.filterWithKey notInStackage hackage)
--
--       allPackages :: Map PackageName (Set Spec)
--       allPackages = Map.union allStackagePackages
--                               (Map.fromList [ (PackageName n, Set.singleton (Spec v Map.empty True True True))  | (n,v) <- Map.toList nonStackagePackages ])
--
--       hackagePackagesFile :: FilePath
--       hackagePackagesFile = nixpkgsRepository </> "pkgs/development/haskell-modules/hackage-packages.nix"
--
--       drvs :: Map PackageIdentifier Derivation
--       drvs = Map.unions (runPar (parMap (uncurry (formatPackages hackage nixpkgs targetPlatform)) (Map.toList allPackages)))
--
--   withFile hackagePackagesFile WriteMode $ \h -> do
--     hPutStrLn h "{ stdenv }:\n\nself: {"
--     forM_ (Map.toList drvs) $ \(pkgId, drv) -> do
--       hPutStrLn h $ render $ nest 2 $ hang (doubleQuotes (text (mangle pkgId)) <+> equals) 2 (pPrint drv <> semi)
--       hPutChar h '\n'
--     hPutStrLn h "}"
--
--   mapM_ (formatSnapshot drvs nixpkgsRepository) snapshots
--
-- mangle :: PackageIdentifier -> String
-- mangle (PackageIdentifier (PackageName n) v) = n ++ '_' : [ if c == '.' then '_' else c | c <- display v ]
--
-- formatSnapshot :: Map PackageIdentifier Derivation -> FilePath -> Snapshot -> IO ()
-- formatSnapshot drvs nixpkgsRepository (Snapshot {..}) = do
--   let
--       snapshotId :: String
--       snapshotId = show (pPrint snapshot)
--
--       compilerId :: String
--       compilerId = filter (`notElem` ['.','-']) (display compiler)
--
--       ltsConfigFile :: FilePath
--       ltsConfigFile = nixpkgsRepository </> "pkgs/development/haskell-modules/configuration-" ++ snapshotId ++ ".nix"
--
--   withFile ltsConfigFile WriteMode $ \h -> do
--     hPutStrLn h "{ pkgs }:"
--     hPutStrLn h ""
--     hPutStrLn h "self: super: {"
--     hPutStrLn h "  # our compiler"
--     hPutStrLn h ("  ghc = pkgs.haskell.compiler." ++ compilerId ++ ";")
--     hPutStrLn h ""
--     hPutStrLn h "  # core packaged provided by the compiler"
--     forM_ (Set.toAscList (Map.keysSet corePackages)) $ \(PackageName n) ->
--       unless (n == "ghc") (hPutStrLn h ("  " ++ n ++ " = null;"))
--     hPutStrLn h ""
--     hPutStrLn h "  # Stackage"
--     forM_ (Map.toAscList packages) $ \(PackageName n, spec) -> do
--       let pkgId = PackageIdentifier (PackageName n) (version spec)
--           drv = drvs Map.! pkgId
--       hPutStrLn h ("  \"" ++ n ++ "\" = self.callPackage self.\"" ++ mangle pkgId ++ "\" " ++ render (braces (overrides drv) <> semi))
--     hPutStrLn h "}"
--   return ()
--
-- formatPackages :: Hackage -> Map Identifier (Set Path) -> Platform -> PackageName -> Set Spec -> Map PackageIdentifier Derivation
-- formatPackages hackage nixpkgs targetPlatform pname specs = Map.fromList
--   [ (PackageIdentifier pname (version spec), formatPackage hackage nixpkgs targetPlatform pname spec) | spec <- Set.toList specs ]
--
-- formatPackage :: Hackage -> Map Identifier (Set Path) -> Platform -> PackageName -> Spec -> Derivation
-- formatPackage hackage nixpkgs targetPlatform pname spec =
--   let
--       pkgId :: PackageIdentifier
--       pkgId = PackageIdentifier pname (version spec)
--
--       abort :: String -> a
--       abort msg = error (display pkgId ++ ": " ++ msg)
--
--       nixpkgsResolver :: Identifier -> Maybe Binding
--       nixpkgsResolver = resolve (Map.map (Set.map (over path ("pkgs":))) nixpkgs)
--
--       pkgDescr :: GenericPackageDescription
--       pkgDescr | Just x <- Map.lookup (unPackageName pname) hackage >>= Map.lookup (version spec) = x
--                | otherwise = abort "not on hackage"
--
--       sha256 :: String
--       sha256 | Just x <- lookup "X-Package-SHA256" (customFieldsPD (packageDescription pkgDescr)) = x
--              | otherwise = abort "cannot find hash"
--
--       drv :: Derivation
--       drv = fromGenericPackageDescription
--               (const True)
--               nixpkgsResolver
--               targetPlatform
--               (unknownCompilerInfo (CompilerId GHC (Version [7,10] [])) NoAbiTag)
--               (Map.toList (flagOverrides spec))
--               []
--               pkgDescr
--             & src .~ DerivationSource "url" ("mirror://hackage/" ++ display pkgId ++ ".tar.gz") "" sha256
--             & doCheck .~ runTests spec
--             & Distribution.Nixpkgs.Haskell.runHaddock .~ Stackage.runHaddock spec
--
--   in drv
--
--
-- -- generatePackageSet :: Platform -> Configuration -> Hackage -> Nixpkgs -> ParIO ()
-- -- generatePackageSet targetPlatform config hackage nixpkgs = do
-- --   let
-- --     corePackageSet :: PackageSet
-- --     corePackageSet = Map.fromList [ (name, v) | PackageIdentifier (PackageName name) v <- Set.toList (corePackages config) ]
-- --
-- --     latestVersionSet :: PackageSet
-- --     latestVersionSet = Map.map (Set.findMax . Map.keysSet) hackage
-- --
-- --     defaultPackageOverridesSet :: PackageSet
-- --     defaultPackageOverridesSet = Map.fromList [ (name, resolveConstraint c hackage) | c@(Dependency (PackageName name) _) <- defaultPackageOverrides config ]
-- --
-- --     generatedDefaultPackageSet :: PackageSet
-- --     generatedDefaultPackageSet = (defaultPackageOverridesSet `Map.union` latestVersionSet) `Map.difference` corePackageSet
-- --
-- --     latestCorePackageSet :: PackageSet
-- --     latestCorePackageSet = latestVersionSet `Map.intersection` corePackageSet
-- --
-- --     latestOverridePackageSet :: PackageSet
-- --     latestOverridePackageSet = latestVersionSet `Map.intersection` defaultPackageOverridesSet
-- --
-- --     extraPackageSet :: PackageMultiSet
-- --     extraPackageSet = Map.unionsWith Set.union
-- --                         [ Map.singleton name (Set.singleton (resolveConstraint c hackage)) | c@(Dependency (PackageName name) _) <- extraPackages config ]
-- --
-- --     db :: PackageMultiSet
-- --     db = Map.unionsWith Set.union [ Map.map Set.singleton generatedDefaultPackageSet
-- --                                   , Map.map Set.singleton latestCorePackageSet
-- --                                   , Map.map Set.singleton latestOverridePackageSet
-- --                                   , extraPackageSet
-- --                                   ]
-- --
-- --     haskellResolver :: Dependency -> Bool
-- --     haskellResolver (Dependency (PackageName name) vrange)
-- --       | Just v <- Map.lookup name corePackageSet                = v `withinRange` vrange
-- --       | Just v <- Map.lookup name generatedDefaultPackageSet    = v `withinRange` vrange
-- --       | otherwise                                               = False
-- --
-- --     nixpkgsResolver :: Identifier -> Maybe Binding
-- --     nixpkgsResolver = resolve (Map.map (Set.map (over path ("pkgs":))) nixpkgs)
-- --
-- --     globalPackageMaintainers :: Map PackageName (Set Identifier)
-- --     globalPackageMaintainers = Map.unionsWith Set.union [ Map.singleton p (Set.singleton m) | (m,ps) <- Map.toList (packageMaintainers config), p <- Set.toList ps ]
-- --
-- --   liftIO $ do putStrLn "/* hackage-packages.nix is an auto-generated file -- DO NOT EDIT! */"
-- --               putStrLn ""
-- --               putStrLn "{ pkgs, stdenv, callPackage }:"
-- --               putStrLn ""
-- --               putStrLn "self: {"
-- --               putStrLn ""
-- --   pkgs <- flip parMapM (Map.toAscList db) $ \(name, vs) -> do
-- --     defs <- forM (Set.toAscList vs) $ \pkgversion -> do
-- --       let -- TODO: Include list of broken dependencies in the generated output.
-- --           descr = hackage Map.! name Map.! pkgversion
-- --
-- --           flagAssignment :: FlagAssignment
-- --           flagAssignment = configureCabalFlags (packageId descr)
-- --
-- --           drv' :: Derivation
-- --           drv' = fromGenericPackageDescription haskellResolver nixpkgsResolver targetPlatform (compilerInfo config) flagAssignment [] descr
-- --
-- --           isInDefaultPackageSet :: Bool
-- --           isInDefaultPackageSet = maybe False (== pkgversion) (Map.lookup name generatedDefaultPackageSet)
-- --
-- --           attr :: String
-- --           attr = name ++ if isInDefaultPackageSet then [] else '_' : [ if c == '.' then '_' else c | c <- display pkgversion ]
-- --
-- --           sha256 :: String
-- --           sha256 | Just x <- lookup "X-Package-SHA256" (customFieldsPD (packageDescription descr)) = x
-- --                  | otherwise = error $ display (packageId descr) ++ " has no hash"
-- --
-- --       srcSpec <- liftIO $ sourceFromHackage (Certain sha256) (name ++ "-" ++ display pkgversion)
-- --
-- --       let drv = drv' & src .~ srcSpec
-- --                      & metaSection.hydraPlatforms %~ (`Set.difference` fromMaybe Set.empty (Map.lookup (PackageName name) (dontDistributePackages config)))
-- --                      & metaSection.hydraPlatforms %~ (if isInDefaultPackageSet then id else const Set.empty)
-- --                      & metaSection.maintainers .~ fromMaybe Set.empty (Map.lookup (PackageName name) globalPackageMaintainers)
-- --
-- --           isFromHackage :: Binding -> Bool
-- --           isFromHackage b = case view (reference . path) b of
-- --                               ["self",_] -> True
-- --                               _ -> False
-- --
-- --           overrides :: Doc
-- --           overrides = fcat $ punctuate space
-- --                         [ pPrint b | b <- Set.toList (view (dependencies . each) drv), not (isFromHackage b) ]
-- --
-- --       return $ nest 2 $ hang (doubleQuotes (text  attr) <+> equals <+> text "callPackage") 2 (parens (pPrint drv)) <+> (braces overrides <> semi)
-- --     return (intercalate "\n\n" (map render defs))
-- --
-- --   liftIO $ mapM_ (\pkg -> putStrLn pkg >> putStrLn "") pkgs
-- --   liftIO $ putStrLn "}"
--
-- -- readPreferredVersions :: FilePath -> IO [Constraint]
-- -- readPreferredVersions p = mapMaybe parsePreferredVersionsLine . lines <$> readFile p
--
-- -- parsePreferredVersionsLine :: String -> Maybe Constraint
-- -- parsePreferredVersionsLine [] = Nothing
-- -- parsePreferredVersionsLine ('-':'-':_) = Nothing
-- -- parsePreferredVersionsLine l = simpleParse l `mplus` error ("invalid preferred-versions line: " ++ show l)
--
-- -- enforcePreferredVersions :: [Constraint] -> String -> Map Version GenericPackageDescription
-- --                          -> Map Version GenericPackageDescription
-- -- enforcePreferredVersions cs pkg = Map.filterWithKey (\v _ -> PackageIdentifier (PackageName pkg) v `satisfiesConstraints` cs)
--
-- overrides :: Derivation -> Doc
-- overrides drv = fcat $ punctuate space [ pPrint b | b <- Set.toList (view (dependencies . each) drv), not (isFromHackage b) ]
--
-- isFromHackage :: Binding -> Bool
-- isFromHackage b = case view (reference . path) b of
--                     ["self",_] -> True
--                     _ -> False
