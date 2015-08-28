module Main ( main ) where

import Control.DeepSeq
import Control.Exception
import qualified Data.Map as Map
import qualified Data.Set as Set
import Distribution.Nixpkgs.Haskell.FromCabal.Configuration.GHC7102
import Distribution.Nixpkgs.License
import Distribution.Nixpkgs.Meta
import Internal.Lens
import Test.Hspec

main :: IO ()
main = do
  hspec $ do
    describe "DeepSeq instances work properly for" $ do
      it "License" $ mapM_ hitsBottom [Known undefined, Unknown (Just undefined)]
      it "Meta" $ do mapM_ hitsBottom
                       [ def & homepage .~ undefined
                       , def & description .~ undefined
                       , def & license .~ undefined
                       , def & platforms .~ undefined
                       , def & maintainers .~ undefined
                       , def & broken .~ undefined
                       ]

    describe "Configuration records are consistent" $
      it "No maintained package is marked as \"dont-distribute\"" $
        Map.keysSet (packageMaintainers ghc7102) `Set.intersection` (dontDistributePackages ghc7102) `shouldSatisfy` Set.null

hitsBottom :: NFData a => a -> Expectation
hitsBottom x = evaluate (rnf x) `shouldThrow` anyErrorCall
