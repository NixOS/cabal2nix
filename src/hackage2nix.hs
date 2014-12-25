-- Run: cabal build -j hackage2nix && dist/build/hackage2nix/hackage2nix >~/.nix-defexpr/pkgs/development/haskell-modules/hackage-packages.nix

module Main ( main ) where

import Cabal2Nix.Generate
-- import Cabal2Nix.Name
import Cabal2Nix.Package
import Cabal2Nix.Hackage ( readHashedHackage, Hackage )
import Control.Monad
import Control.Monad.Par.Combinator
import Control.Monad.Par.IO
import Control.Monad.Trans
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Monoid
import Data.Set ( Set )
import qualified Data.Set as Set
import Distribution.NixOS.Derivation.Cabal
import Distribution.NixOS.PrettyPrinting hiding ( (<>) )
import Distribution.NixOS.PackageMap ( PackageMap, readNixpkgPackageMap )
import Distribution.PackageDescription hiding ( buildDepends, extraLibs, buildTools )
import Distribution.Text

main :: IO ()
main = do
  hackage <- readHashedHackage
  nixpkgs <- readNixpkgPackageMap
  runParIO (generatePackageSet hackage nixpkgs)

nixAttr :: String -> Version -> String
nixAttr name _ = name -- ++ "_" ++ [ if c == '.' then '_' else c | c <- display ver ]

generatePackageSet :: Hackage -> PackageMap -> ParIO ()
generatePackageSet db nixpkgs = do
  pkgs <- parMapM generatePackage (Map.toList db)
  liftIO $ putStrLn "/* hackage-packages.nix is an auto-generated file -- DO NOT EDIT! */"
  liftIO $ putStrLn ""
  liftIO $ putStrLn "{ pkgs, stdenv, callPackage }:"
  liftIO $ putStrLn ""
  liftIO $ putStrLn "self: {"
  liftIO $ putStrLn ""
  forM_ pkgs $ \(name, version, drv) -> do
    let selectHackageNames :: Set String -> Set String
        selectHackageNames = Set.intersection (Map.keysSet db)

        selectMissingHackageNames  :: Set String -> Set String
        selectMissingHackageNames = flip Set.difference (Map.keysSet db)

        conflicts :: Set String
        conflicts = Set.difference (selectHackageNames $ Set.fromList (extraLibs drv ++ pkgConfDeps drv)) missing

        conflictOverrides :: Doc
        conflictOverrides | Set.null conflicts = empty
                          | otherwise          = text " inherit (pkgs) " <> hsep (map text (Set.toAscList conflicts)) <> text "; "

        missing :: Set String
        missing = Set.union (Set.fromList (filter (not . isKnownNixpkgAttribute nixpkgs db) (extraLibs drv ++ pkgConfDeps drv ++ buildTools drv)))
                            (selectMissingHackageNames (Set.fromList (buildDepends drv ++ testDepends drv)))

        missingOverrides :: Doc
        missingOverrides | Set.null missing = empty
                         | otherwise        = fcat [ text (' ':dep++" = null;") | dep <- Set.toAscList missing ] <> space

        overrides :: Doc
        overrides = conflictOverrides $+$ missingOverrides

    let drv' = if Set.null missing then drv else drv { metaSection = (metaSection drv) { broken = True } }
    -- when (name /= toNixName' name) $
    --   liftIO $ print $ nest 2 $ (string (toNixName' name) <+> equals <+> text ("self." ++ show (nixAttr name version))) <> semi
    -- liftIO $ print $ nest 2 $ (string (toNixName' name) <+> equals <+> text ("self." ++ show (nixAttr name version))) <> semi
    -- when (name /= toNixName' name) $
    --   liftIO $ print $ nest 2 $ (string name <+> equals <+> text ("self." ++ show (nixAttr name version))) <> semi
    liftIO $ print $ nest 2 $ hang (string (nixAttr name version) <+> equals <+> text "callPackage") 2 (parens (disp drv')) <+> (braces overrides <> semi)
    liftIO $ putStrLn ""
  liftIO $ putStrLn "}"

generatePackage :: (String, Map Version GenericPackageDescription) -> ParIO (String,Version,Derivation)
generatePackage (name, versions) = do
  let latestVersion = Set.findMax (Map.keysSet versions)
      pkgDescription = (Map.!) versions latestVersion
  srcSpec <- liftIO $ sourceFromHackage Nothing (name ++ "-" ++ display latestVersion)
  let drv' = (cabal2nix pkgDescription) { src = srcSpec }
      Just cabalFileHash = lookup "x-cabal-file-hash" (customFieldsPD (packageDescription pkgDescription))
      drv  = drv' { editedCabalFile = if revision drv' == 0 then "" else cabalFileHash }
  return (name, latestVersion, drv)

isKnownNixpkgAttribute :: PackageMap -> Hackage -> String -> Bool
isKnownNixpkgAttribute nixpkgs hackage name
  | '.' `elem` name                     = True
  | Just _ <- Map.lookup name hackage   = True
  | otherwise                           = maybe False goodScope (Map.lookup name nixpkgs)
  where
    goodScope :: Set [String] -> Bool
    goodScope = not . Set.null . Set.intersection (Set.fromList [[], ["xlibs"], ["gnome"]])
