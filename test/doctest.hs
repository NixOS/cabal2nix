module Main ( main ) where

import Test.DocTest

main :: IO ()
main = do
  doctest ["-isrc", "src/cabal2nix.hs"]
  doctest ["-isrc", "src/hackage2nix.hs"]
