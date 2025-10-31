{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Exception
import Control.Lens
import Control.Monad (forM_)
import Data.Char (isAscii, isSpace)
import Data.List (dropWhile, dropWhileEnd)
import Data.String (fromString)
import Language.Nix.Identifier
import System.Exit (ExitCode (..))
import System.Process (callProcess, readCreateProcess, proc)
import Test.Hspec
import Test.QuickCheck
import Text.Parsec.Class (parseM)
import Text.PrettyPrint.HughesPJClass (prettyShow)

main :: IO ()
main = do
  let nixInstantiate = "nix-instantiate"
  nixInstantiateWorks <- catch
    (callProcess nixInstantiate [ "--version" ] >> pure True)
    (\(e :: SomeException) -> pure False)

  spec $ if nixInstantiateWorks then Just nixInstantiate else Nothing

spec :: Maybe FilePath -> IO ()
spec nixInstantiate = hspec $ do
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

    describe "nix-instantiate" $ do
      let nit :: Example a => String -> (String -> a) -> SpecWith (Arg a)
          nit str spec =
            case nixInstantiate of
              Nothing -> xit str  $ spec undefined
              Just exec -> it str $ spec exec

-- TODO: parseM (nix-instantiate (prettyShow i))
      nit "test" $ \exec -> stringIdentProperty $ \str -> ioProperty $ do
        let expAttr = quote str
            extractAttr =
              dropWhileEnd (`elem` "= \n\t")    -- remove "… = "
              . dropWhileEnd (`elem` "null")    -- remove "null"
              . dropWhileEnd (`elem` ";} \n\t") -- remove "…; }"
              . dropWhile (`elem` "{ \n\t")     -- remove "{ …"
            expr = "{" ++ expAttr ++ "=null;}"

        out <- readCreateProcess (proc exec ["--eval", "--strict", "-E", expr]) ""
        pure $ extractAttr out === expAttr

stringIdentProperty :: Testable prop => (String -> prop) -> Property
stringIdentProperty p = property $ \s ->
  '\0' `notElem` s ==> classify (needsQuoting s) "need quoting" $ p s

identProperty :: Testable prop => (Identifier -> prop) -> Property
identProperty p = property $ \i ->
  classify (needsQuoting (from ident # i)) "need quoting" $ p i
