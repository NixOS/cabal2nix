-- | Internal pretty-printing helpers for Nix expressions.

module Distribution.Nix.Util.PrettyPrinting
  ( onlyIf
  , listattr
  , boolattr
  , attr
  , string
  , funargs
  -- * Re-exports from other modules
  , module Text.PrettyPrint.HughesPJClass
  , Text, disp
  )
  where

import Text.PrettyPrint.HughesPJClass
import Distribution.Text ( Text, disp )

attr :: String -> Doc -> Doc
attr n v = text n <+> equals <+> v <> semi

onlyIf :: Bool -> Doc -> Doc
onlyIf b d = if b then d else empty

boolattr :: String -> Bool -> Bool -> Doc
boolattr n p v = if p then attr n (bool v) else empty

listattr :: String -> Doc -> [String] -> Doc
listattr n prefix vs = onlyIf (not (null vs)) $
                sep [ text n <+> equals <+> prefix <+> lbrack,
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
               lbrace <+> fcat (prepunctuate (comma <> text " ") $ map (nest 2) xs),
               rbrace <> colon
             ]
