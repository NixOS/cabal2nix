module Main ( main ) where

import Internal.Lens
import Control.DeepSeq
import Control.Exception
import Distribution.Nixpkgs.License
import Distribution.Nixpkgs.Meta
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

hitsBottom :: NFData a => a -> Expectation
hitsBottom x = evaluate (rnf x) `shouldThrow` anyErrorCall
