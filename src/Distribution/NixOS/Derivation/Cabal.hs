{-# LANGUAGE PatternGuards, CPP #-}
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
  , module Distribution.NixOS.Derivation.Meta
  , module Data.Version
  )
  where

import Distribution.NixOS.Derivation.Meta
import Distribution.NixOS.PrettyPrinting
import Distribution.Text
import Distribution.Package
#ifdef __HADDOCK__
import Distribution.PackageDescription ( PackageDescription )
#endif
import Distribution.PackageDescription ( FlagAssignment, FlagName(..) )
import Data.Version
import Data.List
import Data.Char
import Data.Function
import Text.Regex.Posix hiding ( empty )

-- | A represtation of Nix expressions for building Haskell packages.
-- The data type correspond closely to the definition of
-- 'PackageDescription' from Cabal.
--
-- Note that the "Text" instance definition provides pretty-printing,
-- but no parsing as of now!

data Derivation = MkDerivation
  { pname               :: String
  , version             :: Version
  , sha256              :: String
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
  , phaseOverrides      :: String
  , metaSection         :: Meta
  }
  deriving (Show, Eq, Ord)

instance Text Derivation where
  disp  = renderDerivation
  parse = error "parsing Distribution.NixOS.Derivation.Cabal.Derivation is not supported yet"

instance Package Derivation where
  packageId deriv = PackageIdentifier (PackageName (pname deriv)) (version deriv)

renderDerivation :: Derivation -> Doc
renderDerivation deriv = funargs (map text ("cabal" : inputs)) $$ vcat
  [ text ""
  , text "cabal.mkDerivation" <+> lparen <> text "self" <> colon <+> lbrace
  , nest 2 $ vcat
    [ attr "pname"   $ string (pname deriv)
    , attr "version" $ doubleQuotes (disp (version deriv))
    , attr "sha256"  $ string (sha256 deriv)
    , boolattr "isLibrary" (not (isLibrary deriv) || isExecutable deriv) (isLibrary deriv)
    , boolattr "isExecutable" (not (isLibrary deriv) || isExecutable deriv) (isExecutable deriv)
    , listattr "buildDepends" (buildDepends deriv)
    , listattr "testDepends" (testDepends deriv)
    , listattr "buildTools" (buildTools deriv)
    , listattr "extraLibraries" (extraLibs deriv)
    , listattr "pkgconfigDepends" (pkgConfDeps deriv)
    , onlyIf renderedFlags $ attr "configureFlags" $ doubleQuotes (sep renderedFlags)
    , boolattr "noHaddock" (not (runHaddock deriv)) (not (runHaddock deriv))
    , boolattr "jailbreak" (jailbreak deriv) (jailbreak deriv)
    , boolattr "doCheck" (not (doCheck deriv)) (doCheck deriv)
    , onlyIf (testTarget deriv) $ attr "testTarget" $ string (testTarget deriv)
    , boolattr "hyperlinkSource" (not (hyperlinkSource deriv)) (hyperlinkSource deriv)
    , onlyIf (phaseOverrides deriv) $ vcat ((map text . lines) (phaseOverrides deriv))
    , disp (metaSection deriv)
    ]
  , rbrace <> rparen
  , text ""
  ]
  where
    inputs = nub $ sortBy (compare `on` map toLower) $ filter (/="cabal") $ filter (not . isPrefixOf "self.") $
              buildDepends deriv ++ testDepends deriv ++ buildTools deriv ++ extraLibs deriv ++ pkgConfDeps deriv ++ extraFunctionArgs deriv
    renderedFlags =  [ text "-f" <> (if enable then empty else char '-') <> text f | (FlagName f, enable) <- cabalFlags deriv ]
                  ++ map text (configureFlags deriv)

-- | A very incomplete parser that extracts 'pname', 'version',
-- 'sha256', 'platforms', 'hydraPlatforms', 'maintainers', 'doCheck',
-- 'jailbreak', and 'runHaddock' from the given Nix expression.

parseDerivation :: String -> Maybe Derivation
parseDerivation buf
  | buf =~ "cabal.mkDerivation"
  , [name]    <- buf `regsubmatch` "pname *= *\"([^\"]+)\""
  , [vers']   <- buf `regsubmatch` "version *= *\"([^\"]+)\""
  , Just vers <- simpleParse vers'
  , [sha]     <- buf `regsubmatch` "sha256 *= *\"([^\"]+)\""
  , hplats    <- buf `regsubmatch` "hydraPlatforms *= *([^;]+);"
  , plats     <- buf `regsubmatch` "platforms *= *([^;]+);"
  , maint     <- buf `regsubmatch` "maintainers *= *\\[([^\"]+)]"
  , noHaddock <- buf `regsubmatch` "noHaddock *= *(true|false) *;"
  , jailBreak <- buf `regsubmatch` "jailbreak *= *(true|false) *;"
  , docheck   <- buf `regsubmatch` "doCheck *= *(true|false) *;"
  , hyperlSrc <- buf `regsubmatch` "hyperlinkSource *= *(true|false) *;"
              = Just MkDerivation
                  { pname          = name
                  , version        = vers
                  , sha256         = sha
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
                  , phaseOverrides = ""
                  , metaSection  = Meta
                                   { homepage       = ""
                                   , description    = ""
                                   , license        = Unknown Nothing
                                   , maintainers    = concatMap words maint
                                   , platforms      = concatMap words (map (map (\c -> if c == '+' then ' ' else c)) plats)
                                   , hydraPlatforms = concatMap words (map (map (\c -> if c == '+' then ' ' else c)) hplats)
                                   }
                  }
  | otherwise = Nothing

regsubmatch :: String -> String -> [String]
regsubmatch buf patt = let (_,_,_,x) = f in x
  where f :: (String,String,String,[String])
        f = match (makeRegexOpts compExtended execBlank patt) buf
