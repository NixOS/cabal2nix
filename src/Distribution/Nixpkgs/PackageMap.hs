module Distribution.Nixpkgs.PackageMap where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import Data.List.Split
import qualified Data.Map as Map
import Data.Map.Strict ( Map )
import Data.Set ( Set )
import qualified Data.Set as Set
import System.Process
import Internal.Lens
import Language.Nix.Identifier
import Language.Nix.Path
import Data.Maybe
import Internal.PrettyPrinting

type PackageMap = Map Identifier (Set Path)

readNixpkgPackageMap :: FilePath -> Maybe Path -> IO PackageMap
readNixpkgPackageMap nixpkgs attrpath = fmap identifierSet2PackageMap (readNixpkgSet nixpkgs attrpath)

readNixpkgSet :: FilePath -> Maybe Path -> IO (Set String)
readNixpkgSet nixpkgs attrpath = do
  let extraArgs = maybe [] (\p -> ["-A", render (pPrint p)]) attrpath
  (_, Just h, _, _) <- createProcess (proc "nix-env" (["-qaP", "--json", "-f"++nixpkgs] ++ extraArgs))
                       { std_out = CreatePipe, env = Nothing }  -- TODO: ensure that overrides don't screw up our results
  buf <- LBS.hGetContents h
  let pkgmap :: Either String (Map String JSON.Object)
      pkgmap = JSON.eitherDecode buf
  either fail (return . Map.keysSet) pkgmap

identifierSet2PackageMap :: Set String -> PackageMap
identifierSet2PackageMap pkgset = foldr (uncurry insertIdentifier) Map.empty pkglist
  where
    pkglist :: [(Identifier, Path)]
    pkglist = mapMaybe parsePackage (Set.toList pkgset)

    insertIdentifier :: Identifier -> Path -> PackageMap -> PackageMap
    insertIdentifier i = Map.insertWith Set.union i . Set.singleton

parsePackage :: String -> Maybe (Identifier, Path)
parsePackage x | null x                 = error "Distribution.Nixpkgs.PackageMap.parsepackage: empty string is no valid identifier"
               | xs <- splitOn "." x    = if needsQuoting (head xs)
                                             then Nothing
                                             else Just (create ident (last xs), create path (map (create ident) (init xs)))
