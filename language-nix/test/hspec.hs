module Main (main) where

import Control.Lens
import Control.Monad (forM_)
import Data.Char (isAscii, isSpace)
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
      it "is equivalent to fromString" $
        stringIdentProperty $ \str -> fromString str == ident # str
      it "allows recovering the original string after conversion to Identifier" $
        stringIdentProperty $ \str -> view ident (review ident str) == str
      it "can be used as a setter" $
        stringIdentProperty $ \str -> set ident str undefined == ident # str

    describe "HasParser Identifier" $ do
      it "can parse the result of prettyShow" $
        identProperty $ \i -> parseM "Identifier" (prettyShow i) == Just (i :: Identifier)
      it "can parse the result of quote" $
        stringIdentProperty $ \str -> parseM "Identifier" (quote str) == Just (ident # str)

    describe "nixKeywords" $ do
      it "are quoted" $ forM_ nixKeywords $ \str -> do
        shouldSatisfy str needsQuoting
        prettyShow (ident # str) `shouldBe` "\"" ++ str ++ "\""

    describe "needsQuoting" $ do
      it "if string contains non ASCII characters" $ stringIdentProperty $ \s ->
        any (not . isAscii) s ==> needsQuoting s
      it "if string contains spaces" $ stringIdentProperty $ \s ->
        any isSpace s ==> needsQuoting s
      it "if length is zero" $ shouldSatisfy "" needsQuoting

stringIdentProperty :: Testable prop => (String -> prop) -> Property
stringIdentProperty p = property $ \s ->
  '\0' `notElem` s ==> classify (needsQuoting s) "need quoting" $ p s

identProperty :: Testable prop => (Identifier -> prop) -> Property
identProperty p = property $ \i ->
  classify (needsQuoting (from ident # i)) "need quoting" $ p i
