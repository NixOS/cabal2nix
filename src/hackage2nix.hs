-- Run: cabal build -j hackage2nix && dist/build/hackage2nix/hackage2nix >~/.nix-defexpr/pkgs/development/haskell-modules/hackage-packages.nix-new && mv ~/.nix-defexpr/pkgs/development/haskell-modules/hackage-packages.nix-new ~/.nix-defexpr/pkgs/development/haskell-modules/hackage-packages.nix

module Main ( main ) where

import Cabal2Nix.Generate
import Cabal2Nix.Hackage ( readHashedHackage, Hackage )
-- import Cabal2Nix.Name
import Cabal2Nix.Package
import Control.Monad
-- import Control.Monad.Par.Combinator
-- import Control.Monad.Par.IO
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

main :: IO ()
main = do
  hackage <- readHashedHackage
  nixpkgs <- readNixpkgPackageMap
  generatePackageSet hackage nixpkgs

generatePackageSet :: Hackage -> PackageMap -> IO ()
generatePackageSet hackage nixpkgs = do
  let db = buildPackageSet hackage
  liftIO $ do putStrLn "/* hackage-packages.nix is an auto-generated file -- DO NOT EDIT! */"
              putStrLn ""
              putStrLn "{ pkgs, stdenv, callPackage }:"
              putStrLn ""
              putStrLn "self: {"
              putStrLn ""
  forM_ (Map.toList db) $ \(name, versions) -> do
    let multiVersionPackage :: Bool
        multiVersionPackage = Map.size versions > 1

        latestVersion :: Version
        latestVersion = fst (Map.findMax versions)

    forM_ (Map.toList versions) $ \(version, descr) -> do
      (drv, overrides) <- generatePackage db nixpkgs name version descr

      let nixAttr = name ++ if multiVersionPackage then "_" ++ [ if c == '.' then '_' else c | c <- display version ] else ""

      liftIO $ do print $ nest 2 $ hang ((string nixAttr) <+> equals <+> text "callPackage") 2 (parens (disp drv)) <+> (braces overrides <> semi)
                  putStrLn ""
      when (multiVersionPackage && version == latestVersion) $
        liftIO $ print $ nest 2 $ (string name <+> equals <+> text ("self." ++ show nixAttr)) <> semi

  liftIO $ putStrLn "}"

generatePackage :: Hackage -> PackageMap -> String -> Version -> GenericPackageDescription -> IO (Derivation, Doc)
generatePackage hackage nixpkgs name version descr = do
  srcSpec <- liftIO $ sourceFromHackage Nothing (name ++ "-" ++ display version)
  let Just cabalFileHash = lookup "x-cabal-file-hash" (customFieldsPD (packageDescription descr))

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

      drv = (cabal2nix descr) { src = srcSpec
                              , editedCabalFile = if revision drv == 0 then "" else cabalFileHash
                              }

  return $ (drv { metaSection = (metaSection drv) { broken = not (Set.null missing) } }, overrides)

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
