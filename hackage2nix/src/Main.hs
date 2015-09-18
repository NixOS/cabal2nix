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
import Distribution.Nixpkgs.Haskell as Derivation
import Distribution.Nixpkgs.Haskell.Constraint
import Distribution.Nixpkgs.Haskell.FromCabal
import Distribution.Nixpkgs.Haskell.FromCabal.Configuration as Config
import Distribution.Nixpkgs.Haskell.FromCabal.Flags
import Distribution.Nixpkgs.Meta
import Distribution.Nixpkgs.PackageMap
import Distribution.Package
import Distribution.PackageDescription hiding ( options, buildDepends, extraLibs, buildTools )
import Distribution.System
import Distribution.Text
import Distribution.Version
import HackageGit
import Language.Nix
import Options.Applicative
import qualified Paths_hackage2nix as Main
import Stackage
import System.FilePath
import System.IO
import Text.PrettyPrint.HughesPJClass hiding ( (<>) )

type PackageSet = Map PackageName Version
type PackageMultiSet = Map PackageName (Set Version)

data CLI = CLI
  { hackageRepository :: FilePath
  , preferredVersionsFile :: Maybe FilePath
  , nixpkgsRepository :: FilePath
  , ltsHaskellRepository :: FilePath
  , stackageNightlyRepository :: FilePath
  , configFile :: FilePath
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
        <*> strOption (long "lts-haskell" <> help "path to LTS Haskell repository" <> value "lts-haskell" <> showDefaultWith id <> metavar "PATH")
        <*> strOption (long "stackage-nightly" <> help "path to Stackage Nightly repository" <> value "stackage-nightly" <> showDefaultWith id <> metavar "PATH")
        <*> strOption (long "config" <> help "path to configuration file inside of Nixpkgs" <> value "pkgs/development/haskell-modules/configuration-hackage2nix.yaml" <> showDefaultWith id <> metavar "PATH")
        <*> option (fmap fromString str) (long "platform" <> help "target platform to generate package set for" <> value "x86_64-linux" <> showDefaultWith display <> metavar "PLATFORM")

      pinfo :: ParserInfo CLI
      pinfo = info
              (   helper
              <*> infoOption ("hackage2nix " ++ display Main.version) (long "version" <> help "Show version number")
              <*> cliOptions
              )
              (  fullDesc
              <> header "hackage2nix converts a Hackage database into a hackage-packages.nix file."
              )
  CLI {..} <- execParser pinfo

  config' <- readConfiguration (nixpkgsRepository </>  configFile)
  nixpkgs <- readNixpkgPackageMap nixpkgsRepository Nothing
  preferredVersions <- readPreferredVersions (fromMaybe (hackageRepository </> "preferred-versions") preferredVersionsFile)
  let fixup = Map.delete "acme-everything"      -- TODO: https://github.com/NixOS/cabal2nix/issues/164
            . Map.delete "som"                  -- TODO: https://github.com/NixOS/cabal2nix/issues/164
            . Map.delete "type"                 -- TODO: https://github.com/NixOS/cabal2nix/issues/163
            . Map.delete "dictionary-sharing"   -- TODO: https://github.com/NixOS/cabal2nix/issues/175
  hackage <- fixup <$> readHackage hackageRepository
  snapshots <- runParIO (readLTSHaskell ltsHaskellRepository)
  nightly <- readStackageNightly stackageNightlyRepository
  let
      config :: Configuration
      config = config' { defaultPackageOverrides = [ Dependency n (thisVersion (Stackage.version spec)) | (n, spec) <- Map.toList (packages nightly) ] }

      hackagePackagesFile :: FilePath
      hackagePackagesFile = nixpkgsRepository </> "pkgs/development/haskell-modules/hackage-packages.nix"

      corePackageSet :: PackageSet
      corePackageSet = Map.fromList [ (name, v) | PackageIdentifier name v <- Set.toList (Config.corePackages config) ]

      latestVersionSet :: PackageSet
      latestVersionSet = Map.map Set.findMax (Map.filter (not . Set.null) (Map.mapWithKey (enforcePreferredVersions preferredVersions) hackage))

      defaultPackageOverridesSet :: PackageSet
      defaultPackageOverridesSet = Map.fromList [ (name, resolveConstraint c hackage) | c@(Dependency name _) <- defaultPackageOverrides config ]

      generatedDefaultPackageSet :: PackageSet
      generatedDefaultPackageSet = (defaultPackageOverridesSet `Map.union` latestVersionSet) `Map.difference` corePackageSet

      latestCorePackageSet :: PackageSet
      latestCorePackageSet = latestVersionSet `Map.intersection` corePackageSet

      latestOverridePackageSet :: PackageSet
      latestOverridePackageSet = latestVersionSet `Map.intersection` defaultPackageOverridesSet

      extraPackageSet :: PackageMultiSet
      extraPackageSet = Map.unionsWith Set.union
                          [ Map.singleton name (Set.singleton (resolveConstraint c hackage)) | c@(Dependency name _) <- extraPackages config ]

      stackagePackageSet :: Map PackageName (Map Version Spec)
      stackagePackageSet = Map.fromListWith Map.union [ (n, Map.singleton (Stackage.version spec) spec) | snapshot <- nightly:snapshots, (n, spec) <- Map.toList (packages snapshot) ]

      db :: PackageMultiSet
      db = Map.unionsWith Set.union [ Map.map Set.singleton generatedDefaultPackageSet
                                    , Map.map Set.singleton latestCorePackageSet
                                    , Map.map Set.singleton latestOverridePackageSet
                                    , extraPackageSet
                                    , Map.map Map.keysSet stackagePackageSet
                                    ]

      haskellResolver :: Dependency -> Bool
      haskellResolver (Dependency name vrange)
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

      let isInDefaultPackageSet :: Bool
          isInDefaultPackageSet = maybe False (== v) (Map.lookup name generatedDefaultPackageSet)

          tarballSHA256 :: SHA256Hash
          tarballSHA256 = fromMaybe (error (display pkgId ++ ": meta data has no hash for the tarball"))
                                    (view (hashes . at "SHA256") meta)

          spec :: Spec
          spec = Spec v mempty True True True `fromMaybe` (Map.lookup name stackagePackageSet >>= Map.lookup v)

          flagAssignment :: FlagAssignment                  -- We don't use the flags from Stackage Nightly here, because
          flagAssignment = configureCabalFlags pkgId        -- they are chosen specifically for GHC 7.10.2.

          attr :: String
          attr = if isInDefaultPackageSet then unPackageName name else mangle pkgId

          drv :: Derivation
          drv = fromGenericPackageDescription haskellResolver nixpkgsResolver targetPlatform (compilerInfo config) flagAssignment [] descr
                  & src .~ DerivationSource "url" ("mirror://hackage/" ++ display pkgId ++ ".tar.gz") "" tarballSHA256
                  & editedCabalFile .~ cabalSHA256
                  & Derivation.runHaddock &&~ Stackage.runHaddock spec
                  & doCheck &&~ runTests spec
                  & metaSection.hydraPlatforms %~ (`Set.difference` Map.findWithDefault Set.empty name (dontDistributePackages config))
                  & metaSection.maintainers .~ Map.findWithDefault Set.empty name globalPackageMaintainers
                  & metaSection.hydraPlatforms %~ (if isInDefaultPackageSet then id else const Set.empty)

          overrides :: Doc
          overrides = fcat $ punctuate space [ pPrint b | b <- Set.toList (view (dependencies . each) drv), not (isFromHackage b) ]
      return $ render $ nest 2 $
        hang (doubleQuotes (text  attr) <+> equals <+> text "callPackage") 2 (parens (pPrint drv)) <+> (braces overrides <> semi)

    return (intercalate "\n\n" defs)

  withFile hackagePackagesFile WriteMode $ \h -> do
    hPutStrLn h "/* hackage-packages.nix is an auto-generated file -- DO NOT EDIT! */"
    hPutStrLn h ""
    hPutStrLn h "{ pkgs, stdenv, callPackage }:"
    hPutStrLn h ""
    hPutStrLn h "self: {"
    hPutStrLn h ""
    mapM_ (\pkg -> hPutStrLn h pkg >> hPutStrLn h "") pkgs
    hPutStrLn h "}"

  void $ runParIO $ flip parMapM snapshots $ \Snapshot {..} -> liftIO $ do
     let allPackages :: PackageSet
         allPackages = Map.difference
                         (Map.fromList [ (name, Stackage.version spec) | (name, spec) <- Map.toList packages ] `Map.union` generatedDefaultPackageSet)
                         corePackages'

         ltsConfigFile :: FilePath
         ltsConfigFile = nixpkgsRepository </> "pkgs/development/haskell-modules/configuration-" ++ show (pPrint snapshot) ++ ".nix"

         corePackages' :: PackageSet
         corePackages' = corePackages `Map.union` Map.fromList [("Cabal", Version [] []), ("rts", Version [] [])]

     withFile ltsConfigFile WriteMode $ \h -> do
       hPutStrLn h "{ pkgs }:"
       hPutStrLn h ""
       hPutStrLn h "with import ./lib.nix { inherit pkgs; };"
       hPutStrLn h ""
       hPutStrLn h ("self: super: assert super.ghc.name == " ++ show (display compiler) ++ "; {")
       hPutStrLn h ""
       hPutStrLn h "  # core libraries provided by the compiler"
       forM_ (Map.keys corePackages') $ \n ->
         unless (n == "ghc") (hPutStrLn h ("  " ++ unPackageName n ++ " = null;"))
       hPutStrLn h ""
       hPutStrLn h ("  # " ++ show (pPrint snapshot) ++ " packages")
       forM_ (Map.toList allPackages) $ \(name, v) -> do
         let pkgId = PackageIdentifier name v

             isInDefaultPackageSet :: Bool
             isInDefaultPackageSet = maybe False (== v) (Map.lookup name generatedDefaultPackageSet)

             isInStackage :: Bool
             isInStackage = isJust (Map.lookup name packages)
         case (isInStackage,isInDefaultPackageSet) of
           (True,True)   -> return ()           -- build is visible and enabled
           (True,False)  -> hPutStrLn h ("  " ++ show (unPackageName name) ++ " = doDistribute super." ++ show (mangle pkgId) ++ ";")
           (False,True)  -> hPutStrLn h ("  " ++ show (unPackageName name) ++ " = dontDistribute super." ++ show (unPackageName name) ++ ";")
           (False,False) -> fail ("logic error processing " ++ display pkgId ++ " in " ++  show (pPrint snapshot))
       hPutStrLn h ""
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
  where msg = "constraint " ++ display c ++ " cannot be resolved in Hackage"

resolveConstraint' :: Constraint -> Hackage -> Maybe Version
resolveConstraint' (Dependency name vrange) hackage
  | Just vset' <- Map.lookup name hackage
  , vset <- Set.filter (`withinRange` vrange) vset'
  , not (Set.null vset)         = Just (Set.findMax vset)
  | otherwise                   = Nothing

mangle :: PackageIdentifier -> String
mangle (PackageIdentifier (PackageName name) v) = name ++ '_' : [ if c == '.' then '_' else c | c <- display v ]
