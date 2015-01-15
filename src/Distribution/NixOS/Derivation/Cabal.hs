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
import Data.Function
import Data.List
import Data.Version
import Distribution.NixOS.Derivation.Meta
import Distribution.NixOS.Fetch
import Distribution.NixOS.Util.PrettyPrinting
import Distribution.Package
import Distribution.PackageDescription ( FlagAssignment, FlagName(..) )
import GHC.Generics ( Generic )

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
  , extraFunctionArgs   :: [String]
  , buildDepends        :: [String]
  , testDepends         :: [String]
  , buildTools          :: [String]
  , extraLibs           :: [String]
  , pkgConfDeps         :: [String]
  , configureFlags      :: [String]
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
renderDerivation deriv =
  funargs (map text ("mkDerivation":inputs)) $$ vcat
  [ text "mkDerivation" <+> lbrace
  , nest 2 $ vcat
    [ attr "pname"   $ string (pname deriv)
    , attr "version" $ doubleQuotes (disp (version deriv))
    , sourceAttr (src deriv)
    , onlyIf (not (null (editedCabalFile deriv))) $ attr "editedCabalFile" $ string (editedCabalFile deriv)
    , boolattr "isLibrary" (not (isLibrary deriv) || isExecutable deriv) (isLibrary deriv)
    , boolattr "isExecutable" (not (isLibrary deriv) || isExecutable deriv) (isExecutable deriv)
    , listattr "buildDepends" empty (buildDepends deriv)
    , listattr "testDepends" empty (testDepends deriv)
    , listattr "buildTools" empty (buildTools deriv)
    , listattr "extraLibraries" empty (extraLibs deriv)
    , listattr "pkgconfigDepends" empty (pkgConfDeps deriv)
    , listattr "configureFlags" empty (map (show . show) renderedFlags)
    , boolattr "enableSplitObjs"  (not (enableSplitObjs deriv)) (enableSplitObjs deriv)
    , boolattr "noHaddock" (not (runHaddock deriv)) (not (runHaddock deriv))
    , boolattr "jailbreak" (jailbreak deriv) (jailbreak deriv)
    , boolattr "doCheck" (not (doCheck deriv)) (doCheck deriv)
    , onlyIf (not (null (testTarget deriv))) $ attr "testTarget" $ string (testTarget deriv)
    , boolattr "hyperlinkSource" (not (hyperlinkSource deriv)) (hyperlinkSource deriv)
    , onlyIf (not (null (phaseOverrides deriv))) $ vcat ((map text . lines) (phaseOverrides deriv))
    , pPrint (metaSection deriv)
    ]
  , rbrace
  ]
  where
    inputs = nub $ sortBy (compare `on` map toLower) $ filter (not . isPrefixOf "stdenv.") $
             buildDepends deriv ++ testDepends deriv ++ buildTools deriv ++ extraLibs deriv ++ pkgConfDeps deriv ++ extraFunctionArgs deriv
             ++ ["fetch" ++ derivKind (src deriv) | derivKind (src deriv) /= "" && not isHackagePackage]
    renderedFlags = [ text "-f" <> (if enable then empty else char '-') <> text f | (FlagName f, enable) <- cabalFlags deriv ]
                    ++ map text (configureFlags deriv)
    isHackagePackage = "mirror://hackage/" `isPrefixOf` derivUrl (src deriv)
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
