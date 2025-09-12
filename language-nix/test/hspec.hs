module Main (main) where

import Control.Lens
import Data.String (fromString)
import Language.Nix.Identifier
import Test.Hspec
import Test.QuickCheck
import Text.Parsec.Class (parseM)
import Text.PrettyPrint.HughesPJClass (prettyShow)

main :: IO ()
main = hspec $ do
  describe "Language.Nix.Identifier" $ do
    describe "ident" $ do
      -- will generate illegal identifiers (with NUL bytes)
      it "is equivalent to fromString" $
        property $ \str -> fromString str == ident # str
      it "allows recovering the original string after conversion to Identifier" $
        property $ \str -> view ident (review ident str) == str
      it "can be used as a setter" $
        property $ \str -> set ident str undefined == ident # str

    describe "HasParser Identifier" $ do
      it "can parse the result of prettyShow" $
        property $ \i -> parseM "Identifier" (prettyShow i) == Just (i :: Identifier)
      -- will generate illegal identifiers (with NUL bytes)
      it "can parse the result of quote" $
        property $ \str -> parseM "Identifier" (quote str) == Just (ident # str)
