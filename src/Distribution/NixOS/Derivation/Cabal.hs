{-# LANGUAGE PatternGuards, RecordWildCards, DeriveGeneric, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}           -- for FlagName below
{- |
   Module      :  Distribution.NixOS.Derivation.Cabal
   License     :  BSD3

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  PatternGuards, RecordWildCards, CPP

   A represtation of Nix expressions based on Cabal builder defined in
   @pkgs\/development\/libraries\/haskell\/cabal\/cabal.nix@.
-}

module Distribution.NixOS.Derivation.Cabal
  ( Derivation(..)
  , renderDerivation
  , DerivationSource(..)
  , module Distribution.NixOS.Derivation.Meta
  , module Data.Version
  )
  where

import Control.DeepSeq.Generics
import Data.Char
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Function
import Data.List
import Data.Version
import Distribution.NixOS.Derivation.Meta
import Distribution.NixOS.Fetch
import Distribution.NixOS.Util.PrettyPrinting
import Distribution.Package
import Distribution.PackageDescription ( FlagAssignment, FlagName(..) )
import GHC.Generics ( Generic )

toAscList :: Set String -> [String]
toAscList = sortBy (compare `on` map toLower) . Set.toList

-- | A represtation of Nix expressions for building Haskell packages.
-- The data type correspond closely to the definition of
-- 'PackageDescription' from Cabal.
--
-- Note that the "Text" instance definition provides pretty-printing,
-- but no parsing as of now!
data Derivation = MkDerivation
  { pname               :: String
  , version             :: Version
  , revision            :: Int
  , src                 :: DerivationSource
  , isLibrary           :: Bool
  , isExecutable        :: Bool
  , extraFunctionArgs   :: Set String
  , buildDepends        :: Set String
  , testDepends         :: Set String
  , buildTools          :: Set String
  , extraLibs           :: Set String
  , pkgConfDeps         :: Set String
  , configureFlags      :: Set String
  , cabalFlags          :: FlagAssignment
  , runHaddock          :: Bool
  , jailbreak           :: Bool
  , doCheck             :: Bool
  , testTarget          :: String
  , hyperlinkSource     :: Bool
  , enableSplitObjs     :: Bool
  , phaseOverrides      :: String
  , editedCabalFile     :: String
  , metaSection         :: Meta
  }
  deriving (Show, Eq, Ord, Generic)

instance Pretty Derivation where
  pPrint  = renderDerivation

instance Package Derivation where
  packageId deriv = PackageIdentifier (PackageName (pname deriv)) (version deriv)

instance NFData Derivation where rnf = genericRnf

deriving instance Generic FlagName
instance NFData FlagName where rnf = genericRnf

renderDerivation :: Derivation -> Doc
renderDerivation (MkDerivation {..}) =
  funargs (map text ("mkDerivation" : toAscList inputs)) $$ vcat
  [ text "mkDerivation" <+> lbrace
  , nest 2 $ vcat
    [ attr "pname"   $ string pname
    , attr "version" $ doubleQuotes (disp version)
    , sourceAttr src
    , onlyIf (not (null editedCabalFile)) $ attr "editedCabalFile" $ string editedCabalFile
    , boolattr "isLibrary" ((not isLibrary) || isExecutable) isLibrary
    , boolattr "isExecutable" ((not isLibrary) || isExecutable) isExecutable
    , listattr "buildDepends" empty (toAscList buildDepends)
    , listattr "testDepends" empty (toAscList testDepends)
    , listattr "buildTools" empty (toAscList buildTools)
    , listattr "extraLibraries" empty (toAscList extraLibs)
    , listattr "pkgconfigDepends" empty (toAscList pkgConfDeps)
    , listattr "configureFlags" empty (map (show . show) renderedFlags)
    , boolattr "enableSplitObjs"  (not enableSplitObjs) enableSplitObjs
    , boolattr "doHaddock" (not runHaddock) runHaddock
    , boolattr "jailbreak" jailbreak jailbreak
    , boolattr "doCheck" (not doCheck) doCheck
    , onlyIf (not (null testTarget)) $ attr "testTarget" $ string testTarget
    , boolattr "hyperlinkSource" (not hyperlinkSource) hyperlinkSource
    , onlyIf (not (null phaseOverrides)) $ vcat ((map text . lines) phaseOverrides)
    , pPrint metaSection
    ]
  , rbrace
  ]
  where
    inputs = Set.unions [ buildDepends, testDepends, buildTools, extraLibs, pkgConfDeps
                        , Set.fromList ["fetch" ++ derivKind src | derivKind src /= "" && not isHackagePackage]
                        ]
    renderedFlags = [ text "-f" <> (if enable then empty else char '-') <> text f | (FlagName f, enable) <- cabalFlags ]
                    ++ map text (toAscList configureFlags)
    isHackagePackage = "mirror://hackage/" `isPrefixOf` derivUrl src
    sourceAttr (DerivationSource{..})
      | isHackagePackage = attr "sha256" $ string derivHash
      | derivKind /= "" = vcat
         [ text "src" <+> equals <+> text ("fetch" ++ derivKind) <+> lbrace
         , nest 2 $ vcat
           [ attr "url" $ string derivUrl
           , attr "sha256" $ string derivHash
           , if derivRevision /= "" then attr "rev" (string derivRevision) else empty
           ]
         , rbrace <> semi
         ]
      | otherwise = attr "src" $ text derivUrl
