module Distribution.Nixpkgs.PackageMap where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import Data.List.Split
import qualified Data.Map as Map
import Data.Map.Strict ( Map )
import Data.Set ( Set )
import qualified Data.Set as Set
import System.Process

type Attribute = String
type Path = [Attribute]

readNixpkgAttributeSet :: FilePath -> IO (Set Attribute)
readNixpkgAttributeSet nixpkgs = do
  (_, Just h, _, _) <- createProcess (proc "nix-env" ["-qaP", "--json", "-f"++nixpkgs])
    { std_out = CreatePipe
    }
  buf <- LBS.hGetContents h
  let pkgmap :: Either String (Map Attribute JSON.Object)
      pkgmap = JSON.eitherDecode buf
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

readNixpkgPackageMap :: FilePath -> IO PackageMap
readNixpkgPackageMap nixpkgs = fmap attributeSet2PackageMap (readNixpkgAttributeSet nixpkgs)
