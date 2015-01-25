module Main ( main ) where

import Test.DocTest

main :: IO ()
main = do
  let libs = [ "dist/build/autogen/Paths_cabal2nix.hs"
             , "src/Cabal2Nix/CorePackages.hs"
             , "src/Cabal2Nix/Flags.hs"
             , "src/Cabal2Nix/Generate.hs"
             , "src/Cabal2Nix/Hackage.hs"
             , "src/Cabal2Nix/License.hs"
             , "src/Cabal2Nix/Name.hs"
             , "src/Cabal2Nix/Normalize.hs"
             , "src/Cabal2Nix/Package.hs"
             , "src/Cabal2Nix/PostProcess.hs"
             , "src/Cabal2Nix/Version.hs"
             , "src/Distribution/Nix/Derivation/Cabal.hs"
             , "src/Distribution/Nix/Derivation/License.hs"
             , "src/Distribution/Nix/Derivation/Meta.hs"
             , "src/Distribution/Nix/Fetch.hs"
             , "src/Distribution/Nix/PackageMap.hs"
             , "src/Distribution/Nix/Util/PrettyPrinting.hs"
             , "src/Distribution/Nix/Util/Regex.hs"
             ]
  doctest $ "src/cabal2nix.hs" : libs
  doctest $ "src/hackage2nix.hs" : libs
