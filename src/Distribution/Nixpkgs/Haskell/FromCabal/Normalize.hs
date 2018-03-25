{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Nixpkgs.Haskell.FromCabal.Normalize ( normalize, normalizeCabalFlags ) where

import Control.Lens
import Data.Function
import Data.List
import qualified Data.Set as Set
import Data.String
import Distribution.Nixpkgs.CabalCompat
import Distribution.Nixpkgs.Haskell
import Distribution.Nixpkgs.Meta
import Distribution.Package
import Distribution.PackageDescription ( FlagAssignment, mkFlagName, unFlagName )
import Distribution.Simple.Utils ( lowercase )
import Language.Nix hiding ( quote )

normalize :: Derivation -> Derivation
normalize drv = drv
  & over libraryDepends (normalizeBuildInfo (packageName drv))
  & over executableDepends (normalizeBuildInfo (packageName drv))
  & over testDepends (normalizeBuildInfo (packageName drv))
  & over benchmarkDepends (normalizeBuildInfo (packageName drv))
  & over metaSection normalizeMeta
  & over cabalFlags normalizeCabalFlags
  & jailbreak %~ (&& (packageName drv /= "jailbreak-cabal"))

normalizeBuildInfo :: PackageName -> BuildInfo -> BuildInfo
normalizeBuildInfo pname bi = bi
  & haskell %~ Set.filter (\b -> view localName b /= fromString (unPackageName pname))
  & tool %~ Set.filter (\b -> view localName b /= fromString (unPackageName pname))

normalizeMeta :: Meta -> Meta
normalizeMeta meta = meta
  & description %~ normalizeSynopsis
  & platforms %~ Set.intersection allKnownPlatforms
  & hydraPlatforms %~ Set.intersection (meta^.platforms)

normalizeSynopsis :: String -> String
normalizeSynopsis desc
  | null desc                                             = []
  | last desc == '.' && length (filter ('.'==) desc) == 1 = normalizeSynopsis (init desc)
  | otherwise                                             = quote (unwords (words desc))

quote :: String -> String
quote ('\\':c:cs) = '\\' : c : quote cs
quote ('"':cs)    = '\\' : '"' : quote cs
quote (c:cs)      = c : quote cs
quote []          = []

-- | When a flag is specified multiple times, the first occurrence counts. This
-- is counter-intuitive, IMHO, but it's how cabal does it. Flag names are
-- spelled in all lowercase.
--
-- >>> normalizeCabalFlags [(mkFlagName "foo", True), (mkFlagName "FOO", True), (mkFlagName "Foo", False)]
-- [(FlagName "foo",True)]

normalizeCabalFlags :: FlagAssignment -> FlagAssignment
normalizeCabalFlags flags' =
    mkFlagAssignment $ nubBy ((==) `on` fst) (sortBy (compare `on` fst) flags)
  where
    flags = [ (mkFlagName (lowercase (unFlagName n)), b) | (n, b) <- unFlagAssignment flags' ]
