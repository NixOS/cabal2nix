module Main ( main ) where

import Control.DeepSeq
import Control.Exception
import Control.Lens
import Control.Lens.Create
import qualified Data.Map as Map
import qualified Data.Set as Set
import Distribution.Nixpkgs.Haskell.FromCabal.Configuration.GHC7102
import Distribution.Nixpkgs.License
import Distribution.Nixpkgs.Meta
import Test.DocTest
import Test.Hspec

main :: IO ()
main = do
  doctest [ "-isrc", "-optP-include", "-optPdist/build/autogen/cabal_macros.h"
          , "src/Distribution/Nixpkgs/Fetch.hs"
          , "src/Distribution/Nixpkgs/Haskell.hs"
          , "src/Distribution/Nixpkgs/Haskell/BuildInfo.hs"
          , "src/Distribution/Nixpkgs/Haskell/Constraint.hs"
          , "src/Distribution/Nixpkgs/Haskell/Derivation.hs"
          , "src/Distribution/Nixpkgs/Haskell/FromCabal.hs"
          , "src/Distribution/Nixpkgs/Haskell/FromCabal/Configuration.hs"
          , "src/Distribution/Nixpkgs/Haskell/FromCabal/Configuration/GHC7102.hs"
          , "src/Distribution/Nixpkgs/Haskell/FromCabal/Configuration/Maintainers.hs"
          , "src/Distribution/Nixpkgs/Haskell/FromCabal/Flags.hs"
          , "src/Distribution/Nixpkgs/Haskell/FromCabal/License.hs"
          , "src/Distribution/Nixpkgs/Haskell/FromCabal/Name.hs"
          , "src/Distribution/Nixpkgs/Haskell/FromCabal/Normalize.hs"
          , "src/Distribution/Nixpkgs/Haskell/FromCabal/PostProcess.hs"
          , "src/Distribution/Nixpkgs/Haskell/Hackage.hs"
          , "src/Distribution/Nixpkgs/Haskell/HackageGit.hs"
          , "src/Distribution/Nixpkgs/Haskell/OrphanInstances.hs"
          , "src/Distribution/Nixpkgs/License.hs"
          , "src/Distribution/Nixpkgs/Meta.hs"
          , "src/Distribution/Nixpkgs/PackageMap.hs"
          , "src/Distribution/Nixpkgs/Haskell/PackageSourceSpec.hs"
          ]

  hspec $ do
    describe "DeepSeq instances work properly for" $ do
      it "License" $ mapM_ hitsBottom [Known undefined, Unknown (Just undefined)]
      it "Meta" $ mapM_ hitsBottom
                    [ def & homepage .~ undefined
                    , def & description .~ undefined
                    , def & license .~ undefined
                    , def & platforms .~ undefined
                    , def & maintainers .~ undefined
                    , def & broken .~ undefined
                    ]

    describe "Configuration records are consistent" $
      it "No maintained package is marked as \"dont-distribute\"" $
        Map.keysSet (packageMaintainers ghc7102) `Set.intersection` dontDistributePackages ghc7102 `shouldSatisfy` Set.null

hitsBottom :: NFData a => a -> Expectation
hitsBottom x = evaluate (rnf x) `shouldThrow` anyErrorCall
