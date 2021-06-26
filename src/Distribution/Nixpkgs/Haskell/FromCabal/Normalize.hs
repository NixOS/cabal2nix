{-# LANGUAGE OverloadedStrings #-}

module Distribution.Nixpkgs.Haskell.FromCabal.Normalize ( normalize ) where

import Control.Lens
import qualified Data.Set as Set
import Data.String
import Distribution.Nixpkgs.Haskell
import Distribution.Nixpkgs.Meta
import Distribution.Package
import Language.Nix hiding ( quote )

normalize :: Derivation -> Derivation
normalize drv = drv
  & over libraryDepends (normalizeBuildInfo (packageName drv))
  & over executableDepends (normalizeBuildInfo (packageName drv))
  & over testDepends (normalizeBuildInfo (packageName drv))
  & over benchmarkDepends (normalizeBuildInfo (packageName drv))
  & over metaSection normalizeMeta
  & jailbreak %~ (&& (packageName drv /= "jailbreak-cabal"))

normalizeBuildInfo :: PackageName -> BuildInfo -> BuildInfo
normalizeBuildInfo pname bi = bi
  & haskell %~ Set.filter (\b -> view localName b /= fromString (unPackageName pname))
  & tool %~ Set.filter (\b -> view localName b /= fromString (unPackageName pname))

normalizeMeta :: Meta -> Meta
normalizeMeta meta = meta
  & description %~ normalizeSynopsis
  & hydraPlatforms %~ (if meta^.broken then const (Just Set.empty) else id)

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
