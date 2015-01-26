module Main ( main ) where

import Control.DeepSeq
import Control.Exception
import Data.Set ( empty )
import Distribution.Nixpkgs.License
import Distribution.Nixpkgs.Meta
import Test.Hspec

main :: IO ()
main = do
  hspec $ do
    describe "DeepSeq instances work properly for" $ do
      it "License" $ mapM_ hitsBottom [Known undefined, Unknown (Just undefined)]
      it "Meta" $ do let meta = Meta "" "" (Unknown Nothing) empty empty empty False
                     mapM_ hitsBottom
                       [ meta { _homepage = undefined }
                       , meta { _description = undefined }
                       , meta { _license = undefined }
                       , meta { _platforms = undefined }
                       , meta { _maintainers = undefined }
                       , meta { _broken = undefined }
                       ]

hitsBottom :: NFData a => a -> Expectation
hitsBottom x = evaluate (rnf x) `shouldThrow` anyErrorCall
