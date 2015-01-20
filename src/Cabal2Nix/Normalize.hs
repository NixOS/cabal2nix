{-# LANGUAGE RecordWildCards #-}

module Cabal2Nix.Normalize ( normalize, normalizeList, normalizeCabalFlags ) where

import Cabal2Nix.CorePackages
import Cabal2Nix.Name
import Data.Char
import Data.Function
import Data.List
import Distribution.NixOS.Derivation.Cabal
import Distribution.NixOS.Util.Regex ( regsubmatch )
import Distribution.PackageDescription ( FlagAssignment, FlagName(..) )
import Distribution.Simple.Utils ( lowercase )

normalize :: Derivation -> Derivation
normalize deriv@(MkDerivation {..}) = deriv
  { buildDepends = normalizeNixNames (filter (`notElem` (pname : corePackages)) buildDepends)
  , testDepends  = normalizeNixNames (filter (`notElem` (pname : corePackages)) testDepends)
  , buildTools   = normalizeNixBuildTools (filter (`notElem` coreBuildTools) buildTools)
  , extraLibs    = normalizeNixLibs extraLibs
  , pkgConfDeps  = normalizeNixLibs pkgConfDeps
  , configureFlags = normalizeList configureFlags
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

normalizeNixNames :: [String] -> [String]
normalizeNixNames = normalizeList . map toNixName

normalizeNixLibs :: [String] -> [String]
normalizeNixLibs = normalizeList . concatMap libNixName

normalizeNixBuildTools :: [String] -> [String]
normalizeNixBuildTools = normalizeList . concatMap buildToolNixName

-- |Strip the "self.ghc.meta.platforms" prefix from platform names, filter
-- duplicates, and sort the resulting list alphabetically.
--
-- >>> normalizeMaintainers ["self.stdenv.lib.maintainers.foobar", "foobar"]
-- ["foobar"]
--
-- >>> normalizeMaintainers ["any.prefix.is.recognized.yo", "abc.def"]
-- ["def","yo"]

normalizeMaintainers :: [String] -> [String]
normalizeMaintainers maints = normalizeList
  [ (m `regsubmatch` "^([^.].*\\.)?([^.]+)$") !! 1 | m <- maints ]

normalizePlatforms :: [String] -> [String]
normalizePlatforms [] = ["ghc.meta.platforms"]
normalizePlatforms plats = normalizeList
  [ if '.' `elem` p then p else "stdenv.lib.platforms." ++ p | p <- plats ]


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
