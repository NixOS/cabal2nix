module Distribution.NixOS.PackageMap where

import qualified Data.Aeson as JSON
import Data.List.Split
import qualified Data.Map as Map
import Data.Map.Strict ( Map )
import Data.Set ( Set )
import qualified Data.Set as Set
import qualified Data.String.UTF8 as UTF8 ( fromString, toRep )
import System.Process

type Attribute = String
type Path = [Attribute]

readNixpkgAttributeSet :: IO (Set Attribute)
readNixpkgAttributeSet = do
  buf <- readProcess "nix-env" ["-qaP", "--json"] ""
  let pkgmap :: Either String (Map Attribute JSON.Object)
      pkgmap = JSON.eitherDecodeStrict (UTF8.toRep (UTF8.fromString buf))
  either fail (return . Map.keysSet) pkgmap

parsePackage :: Attribute -> (Attribute, Path)
parsePackage "" = error "empty string is not a valid package name"
parsePackage x  = (last xs, init xs) where xs = splitOn "." x

type PackageMap = Map Attribute (Set Path)

attributeSet2PackageMap :: Set Attribute -> PackageMap
attributeSet2PackageMap pkgset = foldr (uncurry insertAttribute) Map.empty pkglist
  where
    pkglist :: [(Attribute, Path)]
    pkglist = Set.toList (Set.map parsePackage pkgset)

    insertAttribute :: Attribute -> Path -> PackageMap -> PackageMap
    insertAttribute attr = Map.insertWith Set.union attr . Set.singleton

readNixpkgPackageMap :: IO PackageMap
readNixpkgPackageMap = fmap attributeSet2PackageMap readNixpkgAttributeSet
