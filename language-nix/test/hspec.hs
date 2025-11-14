{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Exception
import Control.Lens
import Control.Monad (forM_)
import Data.Char (isAscii, isSpace)
import Data.List (dropWhileEnd)
import Data.String (fromString)
import Language.Nix.Identifier
import System.Process (callProcess, readCreateProcess, proc)
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
      it "parses redundant escape sequences" $
        forM_
          [ ("\"\\f\"", "f")
          , ("\"echo \\$var\"", "echo $var")
          , ("\"\\h\\e\\l\\l\\o\\ \\w\\or\\l\\d\"", "hello world")
          -- \t and \n don't need to be escaped, though it's advisable
          , ("\"only\\ttechnically\nredundant\"", "only\ttechnically\nredundant")
          ]
          $ \(i, e) -> do
            let e' = Just (ident # e)
            parseM "Identifier" i `shouldBe` e'
            parseM "Identifier" ("\"" ++ e ++ "\"") `shouldBe` e'

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
      nixInstantiate <- runIO $ do
        (callProcess nixInstantiateBin [ "--version" ] >> pure (Just nixInstantiateBin))
          `catch` (\(_ :: SomeException) -> pure Nothing)
      let nix :: Example a => String -> (String -> a) -> SpecWith (Arg a)
          nix str spec =
            case nixInstantiate of
              Nothing -> it str $ \_ ->
                pendingWith (nixInstantiateBin ++ " could not be found or executed")
              Just exec -> it str $ spec exec

      nix "accepts our identifiers and reproduces them" $ \exec -> identProperty $ \i -> ioProperty $ do
        let expAttr = prettyShow i
            expr = "{" ++ expAttr ++ "=null;}"

        out <- readCreateProcess (proc exec ["--eval", "--strict", "-E", expr]) ""
        pure $
          extractIdentSyntax out === expAttr
          .&&. parseM "Identifier" (extractIdentSyntax out) === Just i

nixInstantiateBin :: String
nixInstantiateBin = "nix-instantiate"

stringIdentProperty :: Testable prop => (String -> prop) -> Property
stringIdentProperty p = property $ \s ->
  '\0' `notElem` s ==> classify (needsQuoting s) "need quoting" $ p s

identProperty :: Testable prop => (Identifier -> prop) -> Property
identProperty p = property $ \i ->
  classify (needsQuoting (from ident # i)) "need quoting" $ p i

-- | Given the (pretty) printed representation of the Nix value produced by the
--   expression @{ ${ident} = null; }@, for any value of @ident@, extract the
--   part that represents the identifier.
--
--   Note that pretty printing is buggy in some versions of Nix and the result
--   may not actually be valid Nix syntax.
extractIdentSyntax :: String -> String
extractIdentSyntax =
  dropWhileEnd (`elem` "= \n\t")    -- remove "… = "
  . dropWhileEnd (`elem` "null")    -- remove "null"
  . dropWhileEnd (`elem` ";} \n\t") -- remove "…; }"
  . dropWhile (`elem` "{ \n\t")     -- remove "{ …"
