{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}

-- | Internal pretty-printing helpers for Nix expressions.

module Language.Nix.PrettyPrinting
  ( onlyIf
  , toAscList, toAscListSortedOn
  , setattr
  , listattr, listattrDoc
  , boolattr
  , attr
  , string
  , funargs
  -- * Re-exports from other modules
  , module Text.PrettyPrint.HughesPJClass
  )
  where

-- Avoid name clash with Prelude.<> exported by post-SMP versions of base.
#if MIN_VERSION_base(4,11,0)
import Prelude hiding ( (<>) )
#endif
import Data.Char
import Data.Function
import Data.List (sortBy)
import Data.Set ( Set )
import qualified Data.Set as Set
import "pretty" Text.PrettyPrint.HughesPJClass

attr :: String -> Doc -> Doc
attr n v = text n <+> equals <+> v <> semi

onlyIf :: Bool -> Doc -> Doc
onlyIf b d = if b then d else empty

boolattr :: String -> Bool -> Bool -> Doc
boolattr n p v = if p then attr n (bool v) else empty

listattrDoc :: String -> Doc -> [Doc] -> Doc
listattrDoc n prefix vs = onlyIf (not (null vs)) $
  sep [ text n <+> equals <+> prefix <+> lbrack,
        nest 2 $ fsep vs,
        rbrack <> semi
      ]

listattr :: String -> Doc -> [String] -> Doc
listattr n p = listattrDoc n p . map text

setattr :: String -> Doc -> Set String -> Doc
setattr name prefix set = listattrDoc name prefix
  $ map text $ toAscList set

toAscListSortedOn :: (a -> String) -> Set a -> [a]
toAscListSortedOn f = sortBy (compare `on` map toLower . f) . Set.toList

toAscList :: Set String -> [String]
toAscList = toAscListSortedOn id

bool :: Bool -> Doc
bool True  = text "true"
bool False = text "false"

string :: String -> Doc
string = doubleQuotes . quoteString

quoteString :: String -> Doc
quoteString []          = mempty
quoteString ['\\']      = text "\\\\"
quoteString ('\\':x:xs)
  | x `elem` "\"rn$\\t" = text ['\\',x] <> quoteString xs
  | otherwise           = text "\\\\" <> quoteString (x:xs)
quoteString (x:xs)      = char x <> quoteString xs

prepunctuate :: Doc -> [Doc] -> [Doc]
prepunctuate _ []     = []
prepunctuate p (d:ds) = d : map (p <>) ds

funargs :: [Doc] -> Doc
funargs xs = sep [
               lbrace <+> fcat (prepunctuate (comma <> text " ") $ map (nest 2) xs),
               rbrace <> colon
             ]
