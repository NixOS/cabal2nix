-- Run: cabal build -j hackage2nix && dist/build/hackage2nix/hackage2nix >hackage-packages.nix

module Main ( main ) where

import Cabal2Nix.Generate
import Cabal2Nix.Name
import Cabal2Nix.Package
import Control.Monad
import Control.Monad.Par.Combinator
import Control.Monad.Par.IO
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Monoid
import Data.Set ( Set )
import qualified Data.Set as Set
import Distribution.Hackage.DB ( Hackage, readHackage, GenericPackageDescription )
import Distribution.NixOS.Derivation.Cabal
import Distribution.NixOS.Fetch
import Distribution.NixOS.PrettyPrinting hiding ( (<>) )
import Distribution.Text
import Distribution.PackageDescription hiding ( extraLibs )

main :: IO ()
main = readHackage >>= runParIO . generatePackageSet

getEditedCabalFileHash :: String -> Version -> IO String
getEditedCabalFileHash name version =
  fmap (maybe failure (derivHash . fst)) (runMaybeT $ fetchWith (False,"url",[]) srcspec)
  where
    url = "mirror://hackage/" ++ pkgid ++ "/" ++ name ++ ".cabal"
    srcspec = Source url "" Nothing
    pkgid = name ++ "-" ++ display version
    failure = fail $ "cannot determine edited cabal file hash for " ++ pkgid

nixAttr :: String -> Version -> String
nixAttr name _ = name -- ++ "_" ++ [ if c == '.' then '_' else c | c <- display ver ]

generatePackageSet :: Hackage -> ParIO ()
generatePackageSet db = do
  let selectHackageNames :: Set String -> Set String
      selectHackageNames = Set.intersection (Map.keysSet db)
  pkgs <- parMapM generatePackage (Map.toList db)
  liftIO $ putStrLn "/* hackage-packages.nix is an auto-generated file -- DO NOT EDIT! */"
  liftIO $ putStrLn ""
  liftIO $ putStrLn "{ pkgs, stdenv, callPackage }:"
  liftIO $ putStrLn ""
  liftIO $ putStrLn "self: {"
  liftIO $ putStrLn ""
  forM_ pkgs $ \(name, version, drv) -> do
    let conflicts = Set.toAscList $ selectHackageNames $ Set.fromList (extraLibs drv ++ pkgConfDeps drv)
        overrides | null conflicts = empty
                  | otherwise      = text " inherit (pkgs) " <> hsep (map text conflicts) <> text "; "
    -- when (name /= toNixName' name) $
    --   liftIO $ print $ nest 2 $ (string (toNixName' name) <+> equals <+> text ("self." ++ show (nixAttr name version))) <> semi
    -- liftIO $ print $ nest 2 $ (string (toNixName' name) <+> equals <+> text ("self." ++ show (nixAttr name version))) <> semi
    -- when (name /= toNixName' name) $
    --   liftIO $ print $ nest 2 $ (string name <+> equals <+> text ("self." ++ show (nixAttr name version))) <> semi
    liftIO $ print $ nest 2 $ hang (string (nixAttr name version) <+> equals <+> text "callPackage") 2 (parens (disp drv)) <+> (braces overrides <> semi)
    liftIO $ putStrLn ""
  liftIO $ putStrLn "}"

generatePackage :: (String, Map Version GenericPackageDescription) -> ParIO (String,Version,Derivation)
generatePackage (name, versions) = do
  let latestVersion = Set.findMax (Map.keysSet versions)
      pkgDescription = (Map.!) versions latestVersion
  srcSpec <- liftIO $ sourceFromHackage Nothing (name ++ "-" ++ display latestVersion)
  let drv' = (cabal2nix pkgDescription) { src = srcSpec }
  drv <- if revision drv' > 0
           then do cabalFileHash <- liftIO $ getEditedCabalFileHash (pname drv') (version drv')
                   return $ drv' { editedCabalFile = cabalFileHash }
           else return drv'
  return (name, latestVersion, drv)
