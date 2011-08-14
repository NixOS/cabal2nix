module Distribution.NixOS.Derivation.Cabal
  ( Derivation(..)
  , parseDerivation
  , module Distribution.NixOS.Derivation.Meta
  , Version(..), PackageName(..)
  )
  where

import Distribution.NixOS.Derivation.PrettyPrinting
import Distribution.NixOS.Derivation.Meta
import Distribution.Package
import Distribution.Text
import Data.Version
import Data.List
import Text.PrettyPrint
import Text.ParserCombinators.ReadP ( readP_to_S )
import Text.Regex.Posix

data Derivation = MkDerivation
  { pname        :: PackageName
  , version      :: Version
  , sha256       :: String
  , isLibrary    :: Bool
  , isExecutable :: Bool
  , buildDepends :: [String]
  , buildTools   :: [String]
  , extraLibs    :: [String]
  , pkgConfDeps  :: [String]
  , runHaddock   :: Bool
  , metaSection  :: Meta
  }
  deriving (Show, Eq, Ord)

instance Text Derivation where
  disp  = renderDerivation
  parse = error "parsing Distribution.NixOS.Derivation.Cabal.Derivation is not supported yet"

renderDerivation :: Derivation -> Doc
renderDerivation deriv = funargs (map text ("cabal" : inputs)) $$ vcat
  [ text ""
  , text "cabal.mkDerivation" <+> lparen <> text "self" <> colon <+> lbrace
  , nest 2 $ vcat
    [ attr "pname"   $ doubleQuotes (disp (pname deriv))
    , attr "version" $ doubleQuotes (disp (version deriv))
    , attr "sha256"  $ string (sha256 deriv)
    , boolattr "isLibrary" (not (isLibrary deriv) || (isExecutable deriv)) (isLibrary deriv)
    , boolattr "isExecutable" (not (isLibrary deriv) || (isExecutable deriv)) (isExecutable deriv)
    , listattr "buildDepends" (buildDepends deriv)
    , listattr "buildTools" (buildTools deriv)
    , listattr "extraLibraries" (extraLibs deriv)
    , listattr "pkgconfigDepends" (pkgConfDeps deriv)
    , boolattr "noHaddock" (not (runHaddock deriv)) (not (runHaddock deriv))
    , disp (metaSection deriv)
    ]
  , rbrace <> rparen
  , text ""
  ]
  where
    inputs = nub $ sort $ filter (/="cabal") $ buildDepends deriv ++ buildTools deriv ++ extraLibs deriv ++ pkgConfDeps deriv

parseDerivation :: String -> Maybe Derivation
parseDerivation buf
  | buf =~ "cabal.mkDerivation"
  , [name]    <- buf `regsubmatch` "pname *= *\"([^\"]+)\""
  , [vers]    <- buf `regsubmatch` "version *= *\"([^\"]+)\""
  , [sha]     <- buf `regsubmatch` "sha256 *= *\"([^\"]+)\""
  , plats     <- buf `regsubmatch` "platforms *= *([^;]+);"
  , maint     <- buf `regsubmatch` "maintainers *= *\\[([^\"]+)]"
  , noHaddock <- buf `regsubmatch` "noHaddock *= *(true|false) *;"
              = Just $ MkDerivation
                  { pname        = PackageName name
                  , version      = readVersion vers
                  , sha256       = sha
                  , isLibrary    = False
                  , isExecutable = False
                  , buildDepends = []
                  , buildTools   = []
                  , extraLibs    = []
                  , pkgConfDeps  = []
                  , runHaddock   = case noHaddock of "true":[] -> False
                                                     _         -> True
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

readVersion :: String -> Version
readVersion str =
  case [ v | (v,[]) <- readP_to_S parseVersion str ] of
    [ v' ] -> v'
    _      -> error ("invalid version specifier " ++ show str)
