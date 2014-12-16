{-# LANGUAGE PatternGuards, RecordWildCards, CPP #-}
{- |
   Module      :  Distribution.NixOS.Derivation.Cabal
   License     :  BSD3

   Maintainer  :  nix-dev@cs.uu.nl
   Stability   :  provisional
   Portability :  PatternGuards

   A represtation of Nix expressions based on Cabal builder defined in
   @pkgs\/development\/libraries\/haskell\/cabal\/cabal.nix@.
-}

module Distribution.NixOS.Derivation.Cabal
  ( Derivation(..)
  , parseDerivation
  , DerivationSource(..)
  , module Distribution.NixOS.Derivation.Meta
  , module Data.Version
  )
  where

import Control.DeepSeq
import Data.Char
import Data.Function
import Data.List
import Data.Version
import Distribution.NixOS.Derivation.Meta
import Distribution.NixOS.Fetch
import Distribution.NixOS.PrettyPrinting
import Distribution.NixOS.Regex hiding ( empty )
import Distribution.Package
#ifdef __HADDOCK__
import Distribution.PackageDescription ( PackageDescription )
#endif
import Distribution.PackageDescription ( FlagAssignment, FlagName(..) )
import Distribution.Text

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
  deriving (Show, Eq, Ord)

instance Text Derivation where
  disp  = renderDerivation
  parse = error "parsing Distribution.NixOS.Derivation.Cabal.Derivation is not supported yet"

instance Package Derivation where
  packageId deriv = PackageIdentifier (PackageName (pname deriv)) (version deriv)

instance NFData Derivation where
  rnf (MkDerivation a b c d e f g h i j k l m n o p q r t u v w x) =
    a `deepseq` b `deepseq` c `deepseq` d `deepseq` e `deepseq` f `deepseq` g `deepseq`
    h `deepseq` i `deepseq` j `deepseq` k `deepseq` l `deepseq` m `deepseq` n `deepseqFlagAssignment`
    o `deepseq` p `deepseq` q `deepseq` r `deepseq` t `deepseq` u `deepseq` v `deepseq`
    w `deepseq` x `deepseq` ()

-- FlagName has no NFData instance in old version of Cabal.
deepseqFlagAssignment :: FlagAssignment -> a -> a
deepseqFlagAssignment [] b = b
deepseqFlagAssignment ((FlagName n, v):as) b = n `deepseq` v `deepseq` as `deepseqFlagAssignment` b

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
    , onlyIf (not (null renderedFlags)) $ attr "configureFlags" $ doubleQuotes (sep renderedFlags)
    , boolattr "enableSplitObjs"  (not (enableSplitObjs deriv)) (enableSplitObjs deriv)
    , boolattr "noHaddock" (not (runHaddock deriv)) (not (runHaddock deriv))
    , boolattr "jailbreak" (jailbreak deriv) (jailbreak deriv)
    , boolattr "doCheck" (not (doCheck deriv)) (doCheck deriv)
    , onlyIf (not (null (testTarget deriv))) $ attr "testTarget" $ string (testTarget deriv)
    , boolattr "hyperlinkSource" (not (hyperlinkSource deriv)) (hyperlinkSource deriv)
    , onlyIf (not (null (phaseOverrides deriv))) $ vcat ((map text . lines) (phaseOverrides deriv))
    , disp (metaSection deriv)
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

-- | A very incomplete parser that extracts 'pname', 'version',
-- 'sha256', 'platforms', 'hydraPlatforms', 'maintainers', 'doCheck',
-- 'jailbreak', and 'runHaddock' from the given Nix expression.
parseDerivation :: String -> Maybe Derivation
parseDerivation buf
  | buf =~ "cabal.mkDerivation"
  , [name]    <- buf `regsubmatch` "pname *= *\"([^\"]+)\""
  , [vers']   <- buf `regsubmatch` "version *= *\"([^\"]+)\""
  , Just vers <- simpleParse vers'
  , sha       <- buf `regsubmatch` "sha256 *= *\"([^\"]+)\""
  , sr        <- buf `regsubmatch` "src *= *([^;]+);"
  , url       <- buf `regsubmatch` "url *= *\"?([^;\"]+)\"? *;"
  , rev       <- buf `regsubmatch` "rev *= *\"([^\"]+)\""
  , hplats    <- buf `regsubmatch` "hydraPlatforms *= *([^;]+);"
  , plats     <- buf `regsubmatch` "platforms *= *([^;]+);"
  , maint     <- buf `regsubmatch` "maintainers *= *(with [^;]+;)? \\[([^\"]+)]"
  , noHaddock <- buf `regsubmatch` "noHaddock *= *(true|false) *;"
  , jailBreak <- buf `regsubmatch` "jailbreak *= *(true|false) *;"
  , docheck   <- buf `regsubmatch` "doCheck *= *(true|false) *;"
  , hyperlSrc <- buf `regsubmatch` "hyperlinkSource *= *(true|false) *;"
  , splitObj  <- buf `regsubmatch` "enableSplitObjs *= *(true|false) *;"
  , edtdCabal <- buf `regsubmatch` "editedCabalFile *= *\"([^\"]+)\""
              = Just MkDerivation
                  { pname          = name
                  , version        = vers
                  , revision       = 0
                  , src            = case (sr,url) of
                                       ([]   , _      ) -> DerivationSource "url" ("mirror://hackage/" ++ name ++ "-" ++ vers' ++ ".tar.gz") "" $ head sha
                                       (sr':_, []     ) -> DerivationSource "" sr' "" ""
                                       (sr':_, url':_ ) -> DerivationSource (drop (length "fetch") . head . words $ sr')
                                                                           url' (head $ rev ++ [""]) $ head sha

                  , isLibrary      = False
                  , isExecutable   = False
                  , extraFunctionArgs = []
                  , buildDepends   = []
                  , testDepends    = []
                  , buildTools     = []
                  , extraLibs      = []
                  , pkgConfDeps    = []
                  , configureFlags = []
                  , cabalFlags     = []
                  , runHaddock     = noHaddock /= ["true"]
                  , jailbreak      = jailBreak == ["true"]
                  , doCheck        = docheck == ["true"] || null docheck
                  , testTarget     = ""
                  , hyperlinkSource = hyperlSrc == ["true"] || null hyperlSrc
                  , enableSplitObjs = splitObj  /= ["false"]
                  , phaseOverrides = ""
                  , editedCabalFile = if null edtdCabal then [] else head edtdCabal
                  , metaSection  = Meta
                                   { homepage       = ""
                                   , description    = ""
                                   , license        = Unknown Nothing
                                   , maintainers    = if null maint then [] else concatMap words (tail maint)
                                   , platforms      = concatMap words (map (map (\c -> if c == '+' then ' ' else c)) plats)
                                   , hydraPlatforms = concatMap words (map (map (\c -> if c == '+' then ' ' else c)) hplats)
                                   , broken         = False
                                   }
                  }
  | otherwise = Nothing
