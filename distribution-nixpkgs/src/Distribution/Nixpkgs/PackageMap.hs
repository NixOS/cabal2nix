module Distribution.Nixpkgs.PackageMap
  ( PackageMap, readNixpkgPackageMap
  , resolve
  ) where

import Control.Lens
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import Data.Function
import Data.List (minimumBy)
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set ( Set )
import qualified Data.Set as Set
import Language.Nix
import Paths_distribution_nixpkgs (getDataFileName)
import System.Process

type PackageMap = Map Identifier (Set Path)

-- | Evaluate nixpkgs at a given (nix) path and build a 'Map'
--   keeping track of all 'Path's that end in a given 'Identifier'
--   and evaluate to a derivation.
--   This can be used to find an attribute 'Path' for an arbitrary
--   package name using 'resolve'.
--
--   Note: Evaluation of nixpkgs is very expensive (takes multiple
--   seconds), so cache the result of this function if possible.
--
--   >>> readNixpkgPackageMap "<nixpkgs>" (Just "{ config = { allowAliases = false; }; }")
--   fromList [ … ]
readNixpkgPackageMap :: String
                     -- ^ Path to nixpkgs, must be a valid nix path
                     --   (absolute, relative or @NIX_PATH@ lookup)
                     -> Maybe String
                     -- ^ (Optional) argument attribute set to pass to
                     --   nixpkgs. Must be a valid nix attribute set.
                     -> IO PackageMap
readNixpkgPackageMap nixpkgsPath nixpkgsArgs =
  identifierSet2PackageMap <$> readNixpkgSet nixpkgsPath nixpkgsArgs

readNixpkgSet :: String -> Maybe String -> IO (Set [String])
readNixpkgSet nixpkgsPath nixpkgsArgs = do
  pathsExpr <- getDataFileName "derivation-attr-paths.nix"
  let nixInstantiate = proc "nix-instantiate"
        [ "--strict"
        , "--json"
        , "--eval"
        , pathsExpr
        , "--arg", "nixpkgsPath", nixpkgsPath
        , "--arg", "nixpkgsArgs", fromMaybe "{}" nixpkgsArgs
        ]
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

-- | Finds the shortest 'Path' in a 'PackageMap' that has the
--   given 'Identifier' as its last component.
--
--   >>> resolve nixpkgs (ident # "pam")
--   Just (Bind (Identifier "pam") (Path [Identifier "pam"]))
resolve :: PackageMap -> Identifier -> Maybe Binding
resolve nixpkgs i = case Map.lookup i nixpkgs of
                      Nothing -> Nothing
                      Just ps -> let p = chooseShortestPath (Set.toList ps)
                                 in Just $ binding # (i,p)

chooseShortestPath :: [Path] -> Path
chooseShortestPath [] = error "chooseShortestPath: called with empty list argument"
chooseShortestPath ps = minimumBy (on compare (view (path . to length))) ps
