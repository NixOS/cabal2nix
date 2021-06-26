{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main ( main ) where

import HackageGit

import Control.Lens
import Control.Monad
import Control.Monad.Par.Combinator
import Control.Monad.Par.IO
import Control.Monad.Trans ( liftIO )
import Data.List ( intercalate )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.String
import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.Haskell as Derivation
import Distribution.Nixpkgs.Haskell.Constraint
import Distribution.Nixpkgs.Haskell.FromCabal
import Distribution.Nixpkgs.Haskell.FromCabal.Configuration as Config
import Distribution.Nixpkgs.Haskell.FromCabal.Flags
import Distribution.Nixpkgs.Haskell.OrphanInstances ( )
import Distribution.Nixpkgs.Meta
import Distribution.Nixpkgs.PackageMap
import Distribution.Package
import Distribution.PackageDescription hiding ( options, extraLibs, buildTools, homepage )
import Distribution.System
import Distribution.Types.PackageVersionConstraint
import Distribution.Text
import Distribution.Version
import Language.Nix
import Options.Applicative
import qualified Paths_cabal2nix as Main
import System.FilePath
import System.IO
import Text.PrettyPrint.HughesPJClass hiding ( (<>) )
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (sconcat)
import Options.Applicative.NonEmpty (some1)

type PackageSet = Map PackageName Version
type PackageMultiSet = Map PackageName (Set Version)

data CLI = CLI
  { hackageRepository :: FilePath
  , preferredVersionsFile :: Maybe FilePath
  , nixpkgsRepository :: FilePath
  , configFiles :: NonEmpty FilePath
  , targetPlatform :: Platform
  }
  deriving (Show)

main :: IO ()
main = do
  let cliOptions :: Parser CLI
      cliOptions = CLI
        <$> strOption (long "hackage" <> help "path to Hackage git repository" <> value "hackage" <> showDefaultWith id <> metavar "PATH")
        <*> optional (strOption (long "preferred-versions" <> help "path to Hackage preferred-versions file" <> value "hackage/preferred-versions" <> showDefault <> metavar "PATH"))
        <*> strOption (long "nixpkgs" <> help "path to Nixpkgs repository" <> value "nixpkgs" <> showDefaultWith id <> metavar "PATH")
        <*> some1 (strOption (long "config" <> help "path to configuration file inside of Nixpkgs" <> metavar "PATH"))
        <*> option (fmap fromString str) (long "platform" <> help "target platform to generate package set for" <> value "x86_64-linux" <> showDefaultWith display <> metavar "PLATFORM")

      pinfo :: ParserInfo CLI
      pinfo = info
              (   helper
              <*> infoOption ("hackage2nix " ++ prettyShow Main.version) (long "version" <> help "Show version number")
              <*> cliOptions
              )
              (  fullDesc
              <> header "hackage2nix converts a Hackage database into a hackage-packages.nix file."
              )
  CLI {..} <- execParser pinfo

  config <- sconcat <$> mapM (\file -> readConfiguration (nixpkgsRepository </> file)) configFiles
  nixpkgs <- readNixpkgPackageMap nixpkgsRepository (Just "{ config = { allowAliases = false; }; }")
  preferredVersions <- readPreferredVersions (fromMaybe (hackageRepository </> "preferred-versions") preferredVersionsFile)
  let fixup = Map.delete "acme-everything"      -- TODO: https://github.com/NixOS/cabal2nix/issues/164
            . Map.delete "som"                  -- TODO: https://github.com/NixOS/cabal2nix/issues/164
            . Map.delete "type"                 -- TODO: https://github.com/NixOS/cabal2nix/issues/163
            . Map.delete "control-invariants"   -- TODO: depends on "assert"
            . Map.delete "ConcurrentUtils"      -- TODO: depends on "assert"
            . Map.delete "with"                 -- TODO: https://github.com/NixOS/cabal2nix/issues/164
            . Map.delete "telega"               -- TODO: depends on "with"
            . over (at "hermes") (fmap (set (contains "1.3.4.3") False))  -- TODO: https://github.com/haskell/hackage-server/issues/436
  hackage <- fixup <$> readHackage hackageRepository
  let
      hackagePackagesFile :: FilePath
      hackagePackagesFile = nixpkgsRepository </> "pkgs/development/haskell-modules/hackage-packages.nix"

      corePackageSet :: PackageSet
      corePackageSet = Map.fromList [ (name, v) | PackageIdentifier name v <- Set.toList (Config.corePackages config) ]

      latestVersionSet :: PackageSet
      latestVersionSet = Map.map Set.findMax (Map.filter (not . Set.null) (Map.mapWithKey (enforcePreferredVersions preferredVersions) hackage))

      defaultPackageOverridesSet :: PackageSet
      defaultPackageOverridesSet = Map.fromList [ (name, resolveConstraint c hackage) | c@(PackageVersionConstraint name _) <- defaultPackageOverrides config ]

      generatedDefaultPackageSet :: PackageSet
      generatedDefaultPackageSet = (defaultPackageOverridesSet `Map.union` latestVersionSet) `Map.difference` corePackageSet

      latestCorePackageSet :: PackageSet
      latestCorePackageSet = latestVersionSet `Map.intersection` corePackageSet

      latestOverridePackageSet :: PackageSet
      latestOverridePackageSet = latestVersionSet `Map.intersection` defaultPackageOverridesSet

      extraPackageSet :: PackageMultiSet
      extraPackageSet = Map.unionsWith Set.union
                          [ Map.singleton name (Set.singleton (resolveConstraint c hackage)) | c@(PackageVersionConstraint name _) <- extraPackages config ]

      db :: PackageMultiSet
      db = Map.unionsWith Set.union [ Map.map Set.singleton generatedDefaultPackageSet
                                    , Map.map Set.singleton latestCorePackageSet
                                    , Map.map Set.singleton latestOverridePackageSet
                                    , extraPackageSet
                                    ]

      haskellResolver :: HaskellResolver
      haskellResolver (PackageVersionConstraint name vrange)
        | Just v <- Map.lookup name corePackageSet                = v `withinRange` vrange
        | Just v <- Map.lookup name generatedDefaultPackageSet    = v `withinRange` vrange
        | otherwise                                               = False

      nixpkgsResolver :: Identifier -> Maybe Binding
      nixpkgsResolver = resolve (Map.map (Set.map (over path ("pkgs":))) nixpkgs)

      globalPackageMaintainers :: Map PackageName (Set Identifier)
      globalPackageMaintainers = Map.unionsWith Set.union [ Map.singleton p (Set.singleton m) | (m,ps) <- Map.toList (packageMaintainers config), p <- Set.toList ps ]

  pkgs <- runParIO $ flip parMapM (Map.toAscList db) $ \(name, vs) -> do
    defs <- forM (Set.toAscList vs) $ \v -> liftIO $ do
      let pkgId :: PackageIdentifier
          pkgId = PackageIdentifier name v

      (descr, cabalSHA256) <- readPackage hackageRepository pkgId
      meta <- readPackageMeta hackageRepository pkgId

      let isInDefaultPackageSet, isHydraEnabled, isBroken :: Bool
          isInDefaultPackageSet = (== Just v) (Map.lookup name generatedDefaultPackageSet)
          isHydraEnabled = isInDefaultPackageSet && not (isBroken || name `Set.member` dontDistributePackages config)
          isBroken = any (withinRange v) [ vr | PackageVersionConstraint pn vr <- brokenPackages config, pn == name ]

          tarballSHA256 :: SHA256Hash
          tarballSHA256 = fromMaybe (error (display pkgId ++ ": meta data has no hash for the tarball"))
                                    (view (hashes . at "SHA256") meta)

          flagAssignment :: FlagAssignment                  -- We don't use the flags from Stackage Nightly here, because
          flagAssignment = configureCabalFlags pkgId        -- they are chosen specifically for GHC 7.10.2.

          attr :: String
          attr = if isInDefaultPackageSet then unPackageName name else mangle pkgId

          drv :: Derivation
          drv = fromGenericPackageDescription haskellResolver nixpkgsResolver targetPlatform (compilerInfo config) flagAssignment [] descr
                  & src .~ urlDerivationSource ("mirror://hackage/" ++ display pkgId ++ ".tar.gz") tarballSHA256
                  & editedCabalFile .~ cabalSHA256
                  & metaSection.badPlatforms .~ fmap (Set.map Right) (Map.lookup name (unsupportedPlatforms config))
                  & metaSection.hydraPlatforms %~ (if isHydraEnabled then id else const (Just Set.empty))
                  & metaSection.broken ||~ isBroken
                  & metaSection.maintainers .~ Map.findWithDefault Set.empty name globalPackageMaintainers
                  & metaSection.homepage .~ ""

          overrides :: Doc
          overrides = fcat $ punctuate space [ pPrint b <> semi | b <- Set.toList (view (dependencies . each) drv `Set.union` view extraFunctionArgs drv), not (isFromHackage b) ]
      return $ render $ nest 2 $
        hang (doubleQuotes (text  attr) <+> equals <+> text "callPackage") 2 (parens (pPrint drv)) <+> (braces overrides <> semi)

    return (intercalate "\n\n" defs)

  withFile hackagePackagesFile WriteMode $ \h -> do
    hPutStrLn h "/* hackage-packages.nix is an auto-generated file -- DO NOT EDIT! */"
    hPutStrLn h ""
    hPutStrLn h "{ pkgs, lib, callPackage }:"
    hPutStrLn h ""
    hPutStrLn h "self: {"
    hPutStrLn h ""
    mapM_ (\pkg -> hPutStrLn h pkg >> hPutStrLn h "") pkgs
    hPutStrLn h "}"

isFromHackage :: Binding -> Bool
isFromHackage b = case view (reference . path) b of
                    ["self",_] -> True
                    _ -> False

readPreferredVersions :: FilePath -> IO [Constraint]
readPreferredVersions p = mapMaybe parsePreferredVersionsLine . lines <$> readFile p

parsePreferredVersionsLine :: String -> Maybe Constraint
parsePreferredVersionsLine [] = Nothing
parsePreferredVersionsLine ('-':'-':_) = Nothing
parsePreferredVersionsLine l = simpleParse l `mplus` error ("invalid preferred-versions line: " ++ show l)

enforcePreferredVersions :: [Constraint] -> PackageName -> Set Version -> Set Version
enforcePreferredVersions cs pkg = Set.filter (\v -> PackageIdentifier pkg v `satisfiesConstraints` cs)

resolveConstraint :: Constraint -> Hackage -> Version
resolveConstraint c = fromMaybe (error msg) . resolveConstraint' c
  where msg = unlines [ "constraint " ++ display c ++ " cannot be resolved in Hackage"
                      , "This could be due the package being missing in the hackage directory"
                      , "or the file system not being case sensitive."
                      ]

resolveConstraint' :: Constraint -> Hackage -> Maybe Version
resolveConstraint' (PackageVersionConstraint name vrange) hackage
  | Just vset' <- Map.lookup name hackage
  , vset <- Set.filter (`withinRange` vrange) vset'
  , not (Set.null vset)         = Just (Set.findMax vset)
  | otherwise                   = Nothing

mangle :: PackageIdentifier -> String
mangle (PackageIdentifier name v) = unPackageName name ++ '_' : [ if c == '.' then '_' else c | c <- display v ]
