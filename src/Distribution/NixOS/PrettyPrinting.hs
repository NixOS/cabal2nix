{- |
   Module      :  Distribution.NixOS.PrettyPrinting
   Copyright   :  Peter Simons, Andres Loeh
   License     :  BSD3

   Maintainer  :  nix-dev@cs.uu.nl
   Stability   :  provisional
   Portability :  portable
-}

module Distribution.NixOS.PrettyPrinting
  ( onlyIf
  , listattr
  , boolattr
  , attr
  , string
  , funargs
  , module Text.PrettyPrint
  )
  where

import Text.PrettyPrint

-- Pretty-printing helpers for Nix expressions.

attr :: String -> Doc -> Doc
attr n v = text n <+> equals <+> v <> semi

onlyIf :: [a] -> Doc -> Doc
onlyIf p d = if not (null p) then d else empty

boolattr :: String -> Bool -> Bool -> Doc
boolattr n p v = if p then attr n (bool v) else empty

listattr :: String -> [String] -> Doc
listattr n vs = onlyIf vs $
                sep [ text n <+> equals <+> lbrack,
                      nest 2 $ fsep $ map text vs,
                      rbrack <> semi
                    ]

bool :: Bool -> Doc
bool True  = text "true"
bool False = text "false"

string :: String -> Doc
string = doubleQuotes . text

prepunctuate :: Doc -> [Doc] -> [Doc]
prepunctuate _ []     = []
prepunctuate p (d:ds) = d : map (p <>) ds

funargs :: [Doc] -> Doc
funargs xs = sep [
               lbrace <+> (fcat $ prepunctuate (comma <> text " ") $
                           map (nest 2) xs),
               rbrace <> colon
             ]
