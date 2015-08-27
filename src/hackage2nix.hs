{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main ( main ) where

import Distribution.Nixpkgs.Haskell.FromCabal ( fromGenericPackageDescription )
import Distribution.Nixpkgs.Haskell.FromCabal.Flags
import Distribution.Nixpkgs.Haskell.FromCabal.HackageGit ( readHackage, Hackage )
import Distribution.Nixpkgs.Haskell.Constraint
import Distribution.Nixpkgs.Haskell.FromCabal.Configuration.GHC7102
import Internal.HaskellPackage
import Control.Monad
import Control.Monad.Par.Combinator
import Control.Monad.Par.IO
import Control.Monad.Trans ( liftIO )
import Data.Function
import Data.List
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid
import Data.Set ( Set )
import qualified Data.Set as Set
import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.Haskell
import Distribution.Nixpkgs.Meta
import Distribution.Nixpkgs.PackageMap
import Distribution.PackageDescription hiding ( options, buildDepends, extraLibs, buildTools )
import Distribution.Text
import Distribution.Package
import Distribution.Version
import Internal.Lens
import Internal.PrettyPrinting hiding ( attr, (<>) )
import Internal.Version
import Language.Nix
import Options.Applicative
import System.FilePath

defaultConfiguration :: Configuration
defaultConfiguration = ghc7102

type Nixpkgs = PackageMap       -- Map String (Set [String])
type PackageSet = Map String Version
type PackageMultiSet = Map String (Set Version)

resolveConstraint :: Constraint -> Hackage -> Version
resolveConstraint c = fromMaybe (error ("constraint " ++ display c ++ " cannot be resolved in Hackage")) .
                        resolveConstraint' c

resolveConstraint' :: Constraint -> Hackage -> Maybe Version
resolveConstraint' (Dependency (PackageName name) vrange) hackage | Set.null vs = Nothing
                                                                  | otherwise   = Just (Set.findMax vs)
  where vs = Set.filter (`withinRange` vrange) (Map.keysSet (hackage Map.! name))

data Options = Options
  { hackageRepository :: FilePath
  , preferredVersionsFile :: Maybe FilePath
  , nixpkgsRepository :: FilePath
  }
  deriving (Show)

options :: Parser Options
options = Options
          <$> strOption (long "hackage" <> help "path to Hackage git repository" <> value "hackage" <> showDefault <> metavar "PATH")
          <*> optional (strOption (long "preferred-versions" <> help "path to Hackage preferred-versions file" <> value "hackage/preferred-versions" <> showDefault <> metavar "PATH"))
          <*> strOption (long "nixpkgs" <> help "path to Nixpkgs repository" <> value "<nixpkgs>" <> showDefault <> metavar "PATH")

pinfo :: ParserInfo Options
pinfo = info
        (   helper
        <*> infoOption ("hackage2nix " ++ version) (long "version" <> help "Show version number")
        <*> options
        )
        (  fullDesc
        <> header "hackage2nix converts a Hackage database into a haskell-packages.nix file."
        )

main :: IO ()
main = do
  Options {..} <- execParser pinfo

  hackage <- readHackage hackageRepository
  nixpkgs <- readNixpkgPackageMap nixpkgsRepository Nothing
  preferredVersions <- readPreferredVersions (fromMaybe (hackageRepository </> "preferred-versions") preferredVersionsFile)
  let fixup = Map.delete "acme-everything"      -- TODO: https://github.com/NixOS/cabal2nix/issues/164
            . Map.delete "som"                  -- TODO: https://github.com/NixOS/cabal2nix/issues/164
            . Map.delete "type"                 -- TODO: https://github.com/NixOS/cabal2nix/issues/163
            . Map.delete "dictionary-sharing"   -- TODO: https://github.com/NixOS/cabal2nix/issues/175
            . Map.filter (/= Map.empty)
            . Map.mapWithKey (enforcePreferredVersions preferredVersions)
  runParIO $ generatePackageSet defaultConfiguration (fixup hackage) nixpkgs

enforcePreferredVersions :: [Constraint] -> String -> Map Version GenericPackageDescription
                         -> Map Version GenericPackageDescription
enforcePreferredVersions cs pkg = Map.filterWithKey (\v _ -> PackageIdentifier (PackageName pkg) v `satisfiesConstraints` cs)

generatePackageSet :: Configuration -> Hackage -> Nixpkgs -> ParIO ()
generatePackageSet config hackage nixpkgs = do
  let
    corePackageSet :: PackageSet
    corePackageSet = Map.fromList [ (name, v) | PackageIdentifier (PackageName name) v <- corePackages config ++ hardCorePackages config ]

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
          drv' = fromGenericPackageDescription haskellResolver nixpkgsResolver (platform ghc7102) (compilerInfo ghc7102) flagAssignment [] descr

          attr :: String
          attr | Just v <- Map.lookup name generatedDefaultPackageSet, v == pkgversion = name
               | otherwise = name ++ '_' : [ if c == '.' then '_' else c | c <- display pkgversion ]

          sha256 :: String
          sha256 | Just x <- lookup "X-Package-SHA256" (customFieldsPD (packageDescription descr)) = x
                 | otherwise = error $ display (packageId descr) ++ " has no hash"

      srcSpec <- liftIO $ sourceFromHackage (Certain sha256) (name ++ "-" ++ display pkgversion)

      let drv = drv' & src .~ srcSpec
                     & metaSection.hydraPlatforms .~ (if PackageName name `Set.member` brokenPackages config
                                                         then Set.singleton "stdenv.lib.platforms.none"
                                                         else drv'^.metaSection.hydraPlatforms)

          isFromHackage :: Binding -> Bool
          isFromHackage b = case view (reference . path) b of
                              ["self",_] -> True
                              _ -> False

          overrides :: Doc
          overrides = fcat $ punctuate space
                        [ pPrint b | b <- Set.toList (view (dependencies . each) drv), not (isFromHackage b) ]

      return $ nest 2 $ hang (string attr <+> equals <+> text "callPackage") 2 (parens (pPrint drv)) <+> (braces overrides <> semi)
    return (intercalate "\n\n" (map render defs))

  liftIO $ mapM_ (\pkg -> putStrLn pkg >> putStrLn "") pkgs
  liftIO $ putStrLn "}"

readPreferredVersions :: FilePath -> IO [Constraint]
readPreferredVersions p = mapMaybe parsePreferredVersionsLine . lines <$> readFile p

parsePreferredVersionsLine :: String -> Maybe Constraint
parsePreferredVersionsLine ('-':'-':_) = Nothing
parsePreferredVersionsLine l = case simpleParse l of
                                 Just c -> Just c
                                 Nothing -> error ("invalid preferred-versions line: " ++ show l)
