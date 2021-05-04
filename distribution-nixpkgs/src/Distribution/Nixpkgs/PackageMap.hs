module Distribution.Nixpkgs.PackageMap
  ( PackageMap, readNixpkgPackageMap
  , resolve
  ) where

import Control.Lens
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import Data.Function
import Data.List as List
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set ( Set )
import qualified Data.Set as Set
import Language.Nix
import Paths_distribution_nixpkgs (getDataFileName)
import System.Process

type PackageMap = Map Identifier (Set Path)

readNixpkgPackageMap :: String -> Maybe String -> IO PackageMap
readNixpkgPackageMap nixpkgsPath nixpkgsArgs =
  identifierSet2PackageMap <$> readNixpkgSet nixpkgsPath nixpkgsArgs

readNixpkgSet :: String -> Maybe String -> IO (Set [String])
readNixpkgSet nixpkgsPath nixpkgsArgs = do
  pathsExpr <- getDataFileName "derivation-attr-paths.nix"
  let nixInstantiate = proc "nix-instantiate" $
        [ "--strict"
        , "--json"
        , "--eval"
        , pathsExpr
        , "--arg", "nixpkgsPath", nixpkgsPath
        ] ++ fromMaybe [] (nixpkgsArgs <&> \arg -> ["--arg", "nixpkgsArgs", arg])
  (_, Just h, _, _) <- -- TODO: ensure that overrides don't screw up our results
    createProcess nixInstantiate { std_out = CreatePipe, env = Nothing }
  buf <- LBS.hGetContents h
  either fail return $ JSON.eitherDecode buf

identifierSet2PackageMap :: Set [String] -> PackageMap
identifierSet2PackageMap = foldr insertIdentifier Map.empty
  where
    insertIdentifier :: [String] -> (PackageMap -> PackageMap)
    insertIdentifier rawPath =
      case parsePackage rawPath of
        Nothing -> id
        Just (i, p) -> Map.insertWith Set.union i $ Set.singleton p

parsePackage :: [String] -> Maybe (Identifier, Path)
parsePackage x
  | null x = -- this case would be a bug in derivation-attr-paths.nix
      error "Distribution.Nixpkgs.PackageMap.parsepackage: empty path is no valid identifier"
  | otherwise =
      if any needsQuoting x
        then Nothing
        else Just (ident # last x, path # map (review ident) x)

resolve :: PackageMap -> Identifier -> Maybe Binding
resolve nixpkgs i = case Map.lookup i nixpkgs of
                      Nothing -> Nothing
                      Just ps -> let p = chooseShortestPath (Set.toList ps)
                                 in Just $ binding # (i,p)

chooseShortestPath :: [Path] -> Path
chooseShortestPath [] = error "chooseShortestPath: called with empty list argument"
chooseShortestPath ps = minimumBy (on compare (view (path . to length))) ps
