module Main ( main ) where

import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck
import Control.DeepSeq
import Control.Exception
import Distribution.NixOS.Derivation.License
import Distribution.NixOS.Derivation.Meta

main :: IO ()
main = do
  hspec $ do
    describe "DeepSeq instances work properly for" $ do
      it "License" $ mapM_ hitsBottom [Known undefined, Unknown (Just undefined)]
      it "Meta" $ do let meta = Meta "" "" (Unknown Nothing) [] [] [] False
                     mapM_ hitsBottom
                       [ meta { homepage = undefined }
                       , meta { description = undefined }
                       , meta { license = undefined }
                       , meta { platforms = undefined }
                       , meta { maintainers = undefined }
                       , meta { broken = undefined }
                       ]

hitsBottom :: NFData a => a -> Expectation
hitsBottom x = evaluate (rnf x) `shouldThrow` anyErrorCall
