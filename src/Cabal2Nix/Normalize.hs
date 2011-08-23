{-# LANGUAGE RecordWildCards #-}

module Cabal2Nix.Normalize ( normalize ) where

import Cabal2Nix.License
import Cabal2Nix.PostProcess
import Data.Maybe
import Distribution.Compiler
import qualified Distribution.Package as Cabal
import qualified Distribution.PackageDescription as Cabal
import Distribution.PackageDescription.Configuration
import Distribution.System
import Distribution.Version
import Distribution.NixOS.Derivation.Cabal
import Cabal2Nix.Name
import Cabal2Nix.CorePackages
import Data.List
import Data.Char

normalize :: Derivation -> Derivation
normalize deriv@(MkDerivation {..}) = deriv
  { buildDepends = normalizeNixNames (filter (`notElem` (pname : corePackages)) $ buildDepends)
  , buildTools   = normalizeNixBuildTools (filter (`notElem` coreBuildTools) $ buildTools)
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
  | last desc == '.' && length (filter ('.'==) desc) == 1 = normalizeDescription (reverse (tail (reverse desc)))
  | otherwise                                             = unwords (words desc) >>= quote

quote :: Char -> [Char]
quote '"'  = "\\\""
quote c    = [c]

normalizeList :: [String] -> [String]
normalizeList = nub . sortBy (\x y -> compare (map toLower x) (map toLower y))

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
