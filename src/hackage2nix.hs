-- Run: cabal build -j hackage2nix && dist/build/hackage2nix/hackage2nix >~/.nix-defexpr/pkgs/development/haskell-modules/hackage-packages.nix-new && mv ~/.nix-defexpr/pkgs/development/haskell-modules/hackage-packages.nix-new ~/.nix-defexpr/pkgs/development/haskell-modules/hackage-packages.nix

module Main ( main ) where

import Cabal2Nix.Generate ( cabal2nix' )
import Cabal2Nix.Hackage ( readHashedHackage, Hackage )
-- import Cabal2Nix.Name
import Cabal2Nix.Package
import Cabal2Nix.Flags ( configureCabalFlags )
import Control.Monad
import Control.Monad.Par.Combinator
import Control.Monad.Par.IO
-- import Control.Monad.RWS
import Control.Monad.Trans
-- import Data.Map ( Map )
import qualified Data.Map as Map
-- import Data.Maybe
import Data.Monoid
import Data.Set ( Set )
import qualified Data.Set as Set
-- import Distribution.Compiler
import Distribution.NixOS.Derivation.Cabal
import Distribution.NixOS.PackageMap ( PackageMap, readNixpkgPackageMap )
import Distribution.NixOS.PrettyPrinting hiding ( (<>) )
import Distribution.Package
import Distribution.PackageDescription hiding ( buildDepends, extraLibs, buildTools )
import Distribution.Text
import Distribution.Version
-- import Options.Applicative hiding ( empty )
import Distribution.PackageDescription.Configuration
import Distribution.System
import Distribution.Compiler

main :: IO ()
main = do
  hackage <- readHashedHackage
  nixpkgs <- readNixpkgPackageMap
  runParIO (generatePackageSet hackage nixpkgs)

generatePackageSet :: Hackage -> PackageMap -> ParIO ()
generatePackageSet hackage nixpkgs = do
  let db = buildPackageSet hackage
  liftIO $ do putStrLn "/* hackage-packages.nix is an auto-generated file -- DO NOT EDIT! */"
              putStrLn ""
              putStrLn "{ pkgs, stdenv, callPackage }:"
              putStrLn ""
              putStrLn "self: {"
              putStrLn ""
  pkgs <- (flip parMapM) (Map.toList db) $ \(name, versions) -> do
    let multiVersionPackage :: Bool
        multiVersionPackage = Map.size versions > 1

        latestVersion :: Version
        latestVersion = fst (Map.findMax versions)

    pkg <- forM (Map.toList versions) $ \(version, descr) -> do
      (drv, overrides) <- generatePackage db nixpkgs name version descr

      let nixAttr = name ++ if multiVersionPackage then "_" ++ [ if c == '.' then '_' else c | c <- display version ] else ""

      let def = nest 2 $ hang ((string nixAttr) <+> equals <+> text "callPackage") 2 (parens (disp drv)) <+> (braces overrides <> semi)
      let overr = if multiVersionPackage && version == latestVersion
                  then nest 2 $ (string name <+> equals <+> text ("self." ++ show nixAttr)) <> semi
                  else empty

      return (def $+$ overr)

    return (render (vcat pkg $+$ text ""))

  liftIO $ mapM_ putStrLn pkgs
  liftIO $ putStrLn "}"

generatePackage :: Hackage -> PackageMap -> String -> Version -> GenericPackageDescription -> ParIO (Derivation, Doc)
generatePackage hackage nixpkgs name version descr = do
  srcSpec <- liftIO $ sourceFromHackage Nothing (name ++ "-" ++ display version)
  let Just cabalFileHash = lookup "x-cabal-file-hash" (customFieldsPD (packageDescription descr))

      matches :: PackageIdentifier -> Dependency -> Bool
      matches (PackageIdentifier pn v) (Dependency dn vr) = pn == dn && v `withinRange` vr

      resolver :: Dependency -> Bool
      resolver dep@(Dependency (PackageName pkg) vrange) =
        any (`matches` dep) corePackages ||
        maybe False (not . Map.null . Map.filterWithKey (\k _ -> k `withinRange` vrange)) (Map.lookup pkg hackage)

  -- TODO: Include list of broken dependencies in the generated output.
  (missingDeps,drv') <- case cabal2nix resolver descr of
            Left ds -> do let Right x = cabal2nix (const True) descr
                          return (ds,x)
            Right x -> return ([],x)

  let drv = drv' { src = srcSpec
                 , editedCabalFile = if revision drv == 0 then "" else cabalFileHash
                 }

      selectHackageNames :: Set String -> Set String
      selectHackageNames = Set.intersection (Map.keysSet hackage)

      selectMissingHackageNames  :: Set String -> Set String
      selectMissingHackageNames = flip Set.difference (Map.keysSet hackage)

      conflicts :: Set String
      conflicts = Set.difference (selectHackageNames $ Set.fromList (extraLibs drv ++ pkgConfDeps drv)) missing

      conflictOverrides :: Doc
      conflictOverrides | Set.null conflicts = empty
                        | otherwise          = text " inherit (pkgs) " <> hsep (map text (Set.toAscList conflicts)) <> text "; "

      missing :: Set String
      missing = Set.union (Set.fromList (filter (not . isKnownNixpkgAttribute nixpkgs hackage) (extraLibs drv ++ pkgConfDeps drv ++ buildTools drv)))
                          (selectMissingHackageNames (Set.fromList (buildDepends drv ++ testDepends drv)))

      missingOverrides :: Doc
      missingOverrides | Set.null missing = empty
                       | otherwise        = fcat [ text (' ':dep++" = null;") | dep <- Set.toAscList missing ] <> space

      overrides :: Doc
      overrides = conflictOverrides $+$ missingOverrides

  return $ (drv { metaSection = (metaSection drv) { broken = not (Set.null missing) || not (null missingDeps) } }, overrides)

isKnownNixpkgAttribute :: PackageMap -> Hackage -> String -> Bool
isKnownNixpkgAttribute nixpkgs hackage name
  | '.' `elem` name                     = True
  | Just _ <- Map.lookup name hackage   = True
  | otherwise                           = maybe False goodScope (Map.lookup name nixpkgs)
  where
    goodScope :: Set [String] -> Bool
    goodScope = not . Set.null . Set.intersection (Set.fromList [[], ["xlibs"], ["gnome"]])

buildPackageSet :: Hackage -> Hackage
buildPackageSet db = Map.unionsWith Map.union $
  [ selectLatestMatchingPackage (Dependency (PackageName n) anyVersion) db | n <- Map.keys db ] ++
  [ selectLatestMatchingPackage d db | d <- selectors ]

selectLatestMatchingPackage :: Dependency -> Hackage -> Hackage
selectLatestMatchingPackage (Dependency (PackageName name) vrange) db =
  case Map.lookup name db of
    Nothing -> error (show name ++ " is not a valid Hackage package")
    Just vdb -> let (key,val) = Map.findMax (Map.filterWithKey (\k _ -> k `withinRange` vrange) vdb)
                in  Map.singleton name (Map.singleton key val)

selectors :: [Dependency]
selectors = map (\s -> maybe (error (show s ++ " is not a valid package selector")) id (simpleParse s))
  [ "mtl == 2.1.*"
  , "monad-control == 0.3.*"
  ]

-- data Options = Options
--   { verbose :: Bool
--   , compiler :: CompilerId
--   }
--   deriving (Show)
--
-- data Config = Config
--   { _verbose :: Bool
--   , _hackage :: Hackage
--   , _nixpkgs :: PackageMap
--   }
--   deriving (Show)
--
-- type Compile a = RWST Config () (Set PackageId) ParIO a
--
-- run :: Compile a -> IO a
-- run f = do
--   (a, st, ws) <- runParIO (runRWST f (Config True Map.empty Map.empty) Set.empty)
--   return a
--
-- parseCommandLine :: IO Options
-- parseCommandLine = execParser mainOptions
--   where
--     parseCompilerId :: Parser CompilerId
--     parseCompilerId = option (eitherReader (\s -> maybe (Left (show s ++ " is no valid compiler id")) Right (simpleParse s)))
--                       (  long "compiler"
--                       <> help "identifier of the compiler"
--                       <> metavar "COMPILER-ID"
--                       <> value (fromJust (simpleParse "ghc-7.8.3"))
--                       <> showDefaultWith display
--                       )
--
--     parseOptions :: Parser Options
--     parseOptions = Options
--       <$> switch (long "verbose" <> help "enable detailed progress diagnostics")
--       <*> parseCompilerId
--
--     mainOptions :: ParserInfo Options
--     mainOptions = info (helper <*> parseOptions)
--       (  fullDesc
--          <> header "hackage2nix -- convert the Hackage database into Nix build instructions"
--       )

cabal2nix :: (Dependency -> Bool) -> GenericPackageDescription -> Either [Dependency] Derivation
cabal2nix resolver cabal = case finalize of
  Left ds -> Left ds
  Right (descr, flags) -> Right (cabal2nix' descr) { configureFlags = [ "-f" ++ (if b then "" else "-") ++ n | (FlagName n, b) <- flags ] }
  where
    finalize :: Either [Dependency] (PackageDescription, FlagAssignment)
    finalize = finalizePackageDescription
                 (configureCabalFlags (package (packageDescription cabal)))
                 resolver
                 (Platform X86_64 Linux)                 -- shouldn't be hardcoded
                 (CompilerId GHC (Version [7,8,4] []))   -- dito
                 []
                 cabal

corePackages :: [PackageIdentifier]
corePackages = map (\s -> maybe (error (show s ++ " is not a valid core package")) id (simpleParse s))
  [ "Cabal-1.18.1.5"
  , "array-0.5.0.0"
  , "base-4.7.0.2"
  , "bin-package-db-0.0.0.0"
  , "binary-0.7.1.0"
  , "bytestring-0.10.4.0"
  , "containers-0.5.5.1"
  , "deepseq-1.3.0.2"
  , "directory-1.2.1.0"
  , "filepath-1.3.0.2"
  , "ghc-7.8.4"
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
  , "rts-1.0"
  , "template-haskell-2.9.0.0"
  , "terminfo-0.4.0.0"
  , "time-1.4.2"
  , "transformers-0.3.0.0"
  , "unix-2.7.0.1"
  , "xhtml-3000.2.1"
  ]
