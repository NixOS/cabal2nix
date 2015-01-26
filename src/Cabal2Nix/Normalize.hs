{-# LANGUAGE RecordWildCards #-}

module Cabal2Nix.Normalize ( normalize, normalizeList, normalizeSet, normalizeCabalFlags ) where

import Cabal2Nix.CorePackages
import Cabal2Nix.Name
import Data.Char
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Function
import Data.List
import Distribution.Nixpkgs.Derivation.Cabal
import Distribution.Nixpkgs.Util.Regex ( regsubmatch )
import Distribution.PackageDescription ( FlagAssignment, FlagName(..) )
import Distribution.Simple.Utils ( lowercase )

normalize :: Derivation -> Derivation
normalize deriv@(MkDerivation {..}) = deriv
  { buildDepends = normalizeNixNames (Set.delete pname buildDepends)
  , testDepends  = normalizeNixNames (Set.delete pname testDepends)
  , buildTools   = normalizeNixBuildTools (Set.filter (`notElem` coreBuildTools) buildTools)
  , extraLibs    = normalizeNixLibs extraLibs
  , pkgConfDeps  = normalizeNixLibs pkgConfDeps
  , configureFlags = normalizeSet configureFlags
  , cabalFlags   = normalizeCabalFlags cabalFlags
  , metaSection  = normalizeMeta metaSection
  }

normalizeMeta :: Meta -> Meta
normalizeMeta meta@(Meta {..}) = meta
  { description = normalizeDescription description
  , maintainers = normalizeMaintainers maintainers
  , platforms   = normalizePlatforms platforms
  }

normalizeDescription :: String -> String
normalizeDescription desc
  | null desc                                             = []
  | last desc == '.' && length (filter ('.'==) desc) == 1 = normalizeDescription (init desc)
  | otherwise                                             = quote (unwords (words desc))

quote :: String -> String
quote ('\\':c:cs) = '\\' : c : quote cs
quote ('"':cs)    = '\\' : '"' : quote cs
quote (c:cs)      = c : quote cs
quote []          = []

normalizeList :: [String] -> [String]
normalizeList = nub . sortBy (compare `on` map toLower) . filter (not . null)

normalizeSet :: Set String -> Set String
normalizeSet = Set.filter (not . null)

normalizeNixNames :: Set String -> Set String
normalizeNixNames = normalizeSet . Set.map toNixName

normalizeNixLibs :: Set String -> Set String
normalizeNixLibs = normalizeSet . Set.fromList . concatMap libNixName . Set.toList

normalizeNixBuildTools :: Set String -> Set String
normalizeNixBuildTools = normalizeSet . Set.fromList . concatMap buildToolNixName . Set.toList

-- |Strip any kind of path prefix from maintainer names, filter duplicates, and
-- sort the resulting list alphabetically.
--
-- >>> normalizeMaintainers $ Set.fromList ["self.stdenv.lib.maintainers.foobar", "foobar"]
-- fromList ["foobar"]
--
-- >>> normalizeMaintainers $ Set.fromList ["any.prefix.is.recognized.yo", "abc.def"]
-- fromList ["def","yo"]

normalizeMaintainers :: Set String -> Set String
normalizeMaintainers = normalizeSet . Set.map
  (\m -> (m `regsubmatch` "^([^.].*\\.)?([^.]+)$") !! 1)

normalizePlatforms :: Set String -> Set String
normalizePlatforms = normalizeSet . Set.map
  (\p -> if '.' `elem` p then p else "stdenv.lib.platforms." ++ p)


-- |When a flag is specified multiple times, the first occurrence
-- counts. This is counter-intuitive, IMHO, but it's how cabal does it.
-- Flag names are spelled in all lowercase.
--
-- >>> normalizeCabalFlags [(FlagName "foo", True), (FlagName "FOO", True), (FlagName "Foo", False)]
-- [(FlagName "foo",True)]

normalizeCabalFlags :: FlagAssignment -> FlagAssignment
normalizeCabalFlags flags' = nubBy ((==) `on` fst) (sortBy (compare `on` fst) flags)
  where
    flags = [ (FlagName (lowercase n), b) | (FlagName n, b) <- flags' ]
