module Main ( main ) where

import Test.DocTest
import System.Environment
import Data.Maybe

main :: IO ()
main = do
  distDir <- fromMaybe "dist" `fmap` lookupEnv "HASKELL_DIST_DIR"
  let cabalMacrosHeader = distDir ++ "/build/autogen/cabal_macros.h"
  doctest [ "-isrc", "-optP-include", "-optP"++cabalMacrosHeader, "src" ]
