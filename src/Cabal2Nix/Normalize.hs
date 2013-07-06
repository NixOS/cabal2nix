{-# LANGUAGE RecordWildCards #-}

module Cabal2Nix.Normalize ( normalize, normalizeList ) where

import Distribution.NixOS.Derivation.Cabal
import Cabal2Nix.Name
import Cabal2Nix.CorePackages
import Data.List
import Data.Char
import Data.Function

normalize :: Derivation -> Derivation
normalize deriv@(MkDerivation {..}) = deriv
  { buildDepends = normalizeNixNames (filter (`notElem` (pname : corePackages)) buildDepends)
  , testDepends  = normalizeNixNames (filter (`notElem` (pname : corePackages)) testDepends)
  , buildTools   = normalizeNixBuildTools (filter (`notElem` coreBuildTools) buildTools)
  , extraLibs    = normalizeNixLibs extraLibs
  , pkgConfDeps  = normalizeNixLibs pkgConfDeps
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
normalizeList = nub . sortBy (compare `on` map toLower)

normalizeNixNames :: [String] -> [String]
normalizeNixNames = normalizeList . map toNixName

normalizeNixLibs :: [String] -> [String]
normalizeNixLibs = normalizeList . concatMap libNixName

normalizeNixBuildTools :: [String] -> [String]
normalizeNixBuildTools = normalizeList . concatMap buildToolNixName

normalizeMaintainers :: [String] -> [String]
normalizeMaintainers maints = normalizeList
  [ if '.' `elem` m then m else "self.stdenv.lib.maintainers." ++ m | m <- maints ]

normalizePlatforms :: [String] -> [String]
normalizePlatforms [] = ["self.ghc.meta.platforms"]
normalizePlatforms plats = normalizeList
  [ if '.' `elem` p then p else "self.stdenv.lib.platforms." ++ p | p <- plats ]
