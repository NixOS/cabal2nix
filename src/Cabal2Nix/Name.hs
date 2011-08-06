module Cabal2Nix.Name where

import Data.Char

toNixName :: String -> String
toNixName []      = error "toNixName: empty string is not a valid argument"
toNixName "Cabal" = "cabal"
toNixName name    = f name
  where
    f []                            = []
    f ('-':c:cs) | c `notElem` "-"  = toUpper c : f cs
    f ('-':_)                       = error ("unexpected package name " ++ show name)
    f (c:cs)                        = c : f cs
