{-
   Module      :  Main
   Copyright   :  (c) 2013 Peter Simons
   License     :  BSD3

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  portable

   Cabal2nix doctest suite.
-}

module Main ( main ) where

import Test.DocTest

main :: IO ()
main = do
  let libs = [ "dist/build/autogen/Paths_cabal2nix.hs"
             , "src/Cabal2Nix/License.hs"
             , "src/Cabal2Nix/CorePackages.hs"
             , "src/Cabal2Nix/Flags.hs"
             , "src/Cabal2Nix/Generate.hs"
             , "src/Cabal2Nix/Hackage.hs"
             , "src/Cabal2Nix/Name.hs"
             , "src/Cabal2Nix/Normalize.hs"
             , "src/Cabal2Nix/Package.hs"
             , "src/Cabal2Nix/PostProcess.hs"
             , "src/Cabal2Nix/Version.hs"
             , "src/Distribution/NixOS/Derivation/Cabal.hs"
             , "src/Distribution/NixOS/Derivation/License.hs"
             , "src/Distribution/NixOS/Derivation/Meta.hs"
             , "src/Distribution/NixOS/Fetch.hs"
             , "src/Distribution/NixOS/PrettyPrinting.hs"
             , "src/Distribution/NixOS/Regex.hs"
             ]
  doctest $ "src/cabal2nix.hs" : libs
  doctest $ "src/hackage2nix.hs" : libs
  doctest $ "src/hackage4nix.hs" : libs
