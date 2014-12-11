-- Run: cabal build -j hackage2nix && dist/build/hackage2nix/hackage2nix >hackage-packages.nix

module Main ( main ) where

import Cabal2Nix.Generate
import Cabal2Nix.Name
import Cabal2Nix.Package
import Control.Monad
import Control.Monad.Par.Combinator
import Control.Monad.Par.IO
import Control.Monad.Trans
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Set as Set
import Distribution.Hackage.DB ( Hackage, readHackage, GenericPackageDescription )
import Distribution.NixOS.Derivation.Cabal
import Distribution.NixOS.PrettyPrinting hiding ( (<>) )
import Distribution.Text

main :: IO ()
main = readHackage >>= runParIO . generatePackageSet

nixAttr :: String -> Version -> String
nixAttr name _ = show $ toNixName name -- ++ "_" ++ [ if c == '.' then '_' else c | c <- display ver ]

generatePackageSet :: Hackage -> ParIO ()
generatePackageSet db' = do
  let db = foldr Map.delete db' ["smtLib", "testPkg"]
  pkgs <- parMapM generatePackage (Map.toList db)
  liftIO $ putStrLn "/* hackage-packages.nix is an auto-generated file -- DO NOT EDIT! */"
  liftIO $ putStrLn ""
  liftIO $ putStrLn "{ stdenv, ghc, pkgs, definePackage }:"
  liftIO $ putStrLn ""
  liftIO $ putStrLn "{"
  forM_ pkgs $ \(name, version, nixExpr) -> do
    liftIO $ print $ nest 2 $ hang (text (nixAttr name version) <+> equals <+> text "definePackage {}") 2 (parens (disp nixExpr)) <> semi
    liftIO $ putStrLn ""
  liftIO $ putStrLn "}"

generatePackage :: (String, Map Version GenericPackageDescription) -> ParIO (String,Version,Derivation)
generatePackage (name, versions) = do
  let latestVersion = Set.findMax (Map.keysSet versions)
      pkgDescription = (Map.!) versions latestVersion
  srcSpec <- liftIO $ sourceFromHackage Nothing (name ++ "-" ++ display latestVersion)
  return (name, latestVersion, (cabal2nix pkgDescription) { src = srcSpec })
