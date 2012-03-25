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
  , buildDepends        :: [String]
  , buildTools          :: [String]
  , extraLibs           :: [String]
  , pkgConfDeps         :: [String]
  , configureFlags      :: [String]
  , cabalFlags          :: FlagAssignment
  , runHaddock          :: Bool
  , postInstall         :: String
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
    , boolattr "isLibrary" (not (isLibrary deriv) || (isExecutable deriv)) (isLibrary deriv)
    , boolattr "isExecutable" (not (isLibrary deriv) || (isExecutable deriv)) (isExecutable deriv)
    , listattr "buildDepends" (buildDepends deriv)
    , listattr "buildTools" (buildTools deriv)
    , listattr "extraLibraries" (extraLibs deriv)
    , listattr "pkgconfigDepends" (pkgConfDeps deriv)
    , onlyIf renderedFlags $ attr "configureFlags" $ doubleQuotes (sep renderedFlags)
    , boolattr "noHaddock" (not (runHaddock deriv)) (not (runHaddock deriv))
    , onlyIf (postInstall deriv) $ text (postInstall deriv)
    , disp (metaSection deriv)
    ]
  , rbrace <> rparen
  , text ""
  ]
  where
    inputs = nub $ sortBy (\x y -> compare (map toLower x) (map toLower y)) $ filter (/="cabal") $
              buildDepends deriv ++ buildTools deriv ++ extraLibs deriv ++ pkgConfDeps deriv
    renderedFlags =  [ text "-f" <> (if enable then empty else char '-') <> text f | (FlagName f, enable) <- cabalFlags deriv ]
                  ++ map text (configureFlags deriv)

-- | A very incomplete parser that extracts 'pname', 'version',
-- 'sha256', 'platforms', 'maintainers', and 'runHaddock' from the given
-- Nix expression.

parseDerivation :: String -> Maybe Derivation
parseDerivation buf
  | buf =~ "cabal.mkDerivation"
  , [name]    <- buf `regsubmatch` "pname *= *\"([^\"]+)\""
  , [vers']   <- buf `regsubmatch` "version *= *\"([^\"]+)\""
  , Just vers <- simpleParse vers'
  , [sha]     <- buf `regsubmatch` "sha256 *= *\"([^\"]+)\""
  , plats     <- buf `regsubmatch` "platforms *= *([^;]+);"
  , maint     <- buf `regsubmatch` "maintainers *= *\\[([^\"]+)]"
  , noHaddock <- buf `regsubmatch` "noHaddock *= *(true|false) *;"
              = Just $ MkDerivation
                  { pname          = name
                  , version        = vers
                  , sha256         = sha
                  , isLibrary      = False
                  , isExecutable   = False
                  , buildDepends   = []
                  , buildTools     = []
                  , extraLibs      = []
                  , pkgConfDeps    = []
                  , configureFlags = []
                  , cabalFlags     = []
                  , runHaddock     = case noHaddock of "true":[] -> False
                                                       _         -> True
                  , postInstall    = ""
                  , metaSection  = Meta
                                   { homepage    = ""
                                   , description = ""
                                   , license     = Unknown Nothing
                                   , maintainers = concatMap words maint
                                   , platforms   = concatMap words (map (map (\c -> if c == '+' then ' ' else c)) plats)
                                   }
                  }
  | otherwise = Nothing

regsubmatch :: String -> String -> [String]
regsubmatch buf patt = let (_,_,_,x) = f in x
  where f :: (String,String,String,[String])
        f = match (makeRegexOpts compExtended execBlank patt) buf
