-- self-test.hs

module Main ( main ) where

import Language.Nix
import Text.PrettyPrint.Leijen ( pretty )
import qualified Text.Parsec.Token as Parsec ( reservedNames )
import Test.DocTest
import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.HUnit.Base ( assertFailure, assertEqual )

gives :: (Show err, Eq a, Show a) => Either err a -> a -> Expectation
gives x y = either (assertFailure . msg) (assertEqual "" y) x
  where msg z = "expected: " ++ show y ++ "\nbut got parser error: " ++ show z

parseFail :: Show a => NixParser a -> String -> Expectation
parseFail p input = parse p input `shouldSatisfy` either (const True) (const False)

main :: IO ()
main = do
  doctest [ "Language/Nix.hs" ]
  hspec $ do
    describe "identifier" $ do
      it "parses hand-picked sample inputs" $ do
        parse identifier "abc" `gives` Ident "abc"
        parse identifier "abc  " `gives` Ident "abc"
        parse identifier "__a-b-c-__  " `gives` Ident "__a-b-c-__"
      prop "parses all randomly generated samples" $
        forAll genIdentifier $ \input -> either (const False) (Ident input ==) (parse identifier input)
      it "does not swallow leading whitespace" $
        parseFail identifier " abc"
      it "does not parse a de-referencing expression" $
        parseFail identifier "abc.def"
      it "does not accept reserved words" $
        mapM_ (parseFail identifier) (Parsec.reservedNames nixLanguage)
      it "accepts identifiers that are a prefix of a reserved word" $
        parse identifier "lett" `gives` Ident "lett"

    describe "scopedIdentifier" $ do
      it "parses hand-picked sample inputs" $ do
        parse scopedIdentifier "abc" `gives` SIdent ["abc"]
        parse scopedIdentifier "abc  " `gives` SIdent ["abc"]
        parse scopedIdentifier "abc.def" `gives` SIdent ["abc","def"]
      prop "parses all randomly generated samples" $
        \sident -> either (const False) (sident ==) (parse scopedIdentifier (show (pretty sident)))
      it "does not swallow leading whitespace" $
        parseFail scopedIdentifier " abc"
      it "does not accept reserved words" $
        mapM_ (parseFail scopedIdentifier) (Parsec.reservedNames nixLanguage)
      it "accepts identifiers that are a prefix of a reserved word" $
        parse scopedIdentifier "lett" `gives` SIdent ["lett"]

    describe "literal" $ do
      prop "parses all randomly generated literal strings" $
        \str -> either (const False) (Lit str ==) (parse literal (show str))
      prop "parses all randomly generated integers" $
        \n -> either (const False) (Lit (show (abs (n::Int))) ==) (parse literal (show (abs n)))
      it "parses paths" $
        parse literal "claus/ist/der/beste" `gives` Lit "claus/ist/der/beste"
      it "parses URIs" $
        parse literal "http://example.org" `gives` Lit "http://example.org"
      it "parses antiquotation" $ {- do
        parse literal "\"a${b}c\"" `gives` Lit "a${b}c" -}
        pending ("parse literal " ++ "\"a${if !x then \"b\" else \"c\"}d\"")

    describe "attrSet" $ do
      it "parses an empty attribute set" $ do
        parse attrSet "{}" `gives` AttrSet False []
        parse attrSet "rec {}" `gives` AttrSet True []
      it "parses hand-picked sample inputs" $ do
        parse attrSet "{ a = b; }" `gives` AttrSet False [Assign (SIdent ["a"]) (Ident "b")]
        parse attrSet "{ a = b.c; }" `gives` AttrSet False [Assign (SIdent ["a"]) (Deref (Ident "b") (Ident "c"))]
        parse attrSet "{ a = \"b\"; }" `gives` AttrSet False [Assign (SIdent ["a"]) (Lit "b")]
      it "parses attribute sets as values of attribute sets" $
        parse attrSet "{ a = { b = c; }; }" `gives` AttrSet False [Assign (SIdent ["a"]) (AttrSet False [Assign (SIdent ["b"]) (Ident "c")])]
      it "expects assignments to terminated by a semicolon" $
        parseFail attrSet "{ a = b }"
      it "ignores comments" $
        parse attrSet "{ /* foo */ a = /* bar */ b; # foobar\n }" `gives` AttrSet False [Assign (SIdent ["a"]) (Ident "b")]
      it "parses recursive attribute sets" $
        parse attrSet "rec { a = b; b = a; }" `gives` AttrSet True [Assign (SIdent ["a"]) (Ident "b"), Assign (SIdent ["b"]) (Ident "a")]
      it "parses inherit statements" $ do
        parse attrSet "{ inherit a; }" `gives` AttrSet False [Inherit (SIdent []) ["a"]]
        parse attrSet "{ inherit a; inherit b; }" `gives` AttrSet False [Inherit (SIdent []) ["a"],Inherit (SIdent []) ["b"]]
        parse attrSet "{ inherit a b; }" `gives` AttrSet False [Inherit (SIdent []) ["a","b"]]
        parse attrSet "{ inherit (a) b c d; }" `gives` AttrSet False [Inherit (SIdent ["a"]) ["b","c","d"]];

    describe "list" $ do
      it "parses an empty list" $
        parse list "[]" `gives` List []
      it "parses hand-picked sample inputs" $ do
        parse list "[ a b c ]" `gives` List [Ident "a",Ident "b",Ident "c"]
        parse list "[ \"b\" { a = [\"c\"]; } d ]" `gives` List [Lit "b",AttrSet False [Assign (SIdent ["a"]) (List [Lit "c"])],Ident "d"]
        parse list "[ (a b) c ]" `gives` List [Apply (Ident "a") (Ident "b"),Ident "c"]
        parse list "[ 12 8 a 0 ]" `gives` List [Lit "12", Lit "8", Ident "a", Lit "0"]

    describe "reserved" $ do
      it "parses a specific reserved name" $ do
         parse (reserved "let") "let" `gives` ()
         parse (reserved "in") "in" `gives` ()
         parse (reserved "rec") "rec" `gives` ()
         parseFail (reserved "rec") "let"
      it "recognizes if the keyword is actually just a prefix of the input string" $
         parseFail (reserved "in") "input"

    describe "expr" $ do
      it "parses an empty attribute set" $ do
        parse expr "{}" `gives` AttrSet False []
        parse expr "rec {}" `gives` AttrSet True []
      it "parses an empty list" $
        parse expr "[]" `gives` List []
      it "parses a de-referencing expression" $ do
        parse expr "abc.def" `gives` Deref (Ident "abc") (Ident "def")
        parse expr "a.b.c" `gives` Deref (Deref (Ident "a") (Ident "b")) (Ident "c")
      it "parses recursive attribute sets" $
        parse expr "rec { id = x: x; }" `gives` AttrSet True [Assign (SIdent ["id"]) (Fun (Ident "x") (Ident "x"))]
      it "parses boolean expressions" $ do
        parse expr "true" `gives` Ident "true"
        parse expr "false" `gives` Ident "false"
        parse expr "system == \"linux\"" `gives` Equal (Ident "system") (Lit "linux")
        parse expr "system != \"linux\"" `gives` Inequal (Ident "system") (Lit "linux")
        parse expr "true && true" `gives` And (Ident "true") (Ident "true")
        parse expr "false || false" `gives` Or (Ident "false") (Ident "false")
        parse expr "isLinux || isDarwin" `gives` Or (Ident "isLinux") (Ident "isDarwin")
        parse expr "(isLinux || isDarwin)" `gives` Or (Ident "isLinux") (Ident "isDarwin")
        parse expr "(isLinux) || (isDarwin)" `gives` Or (Ident "isLinux") (Ident "isDarwin")
        parse expr "!(!isLinux) || (isDarwin)" `gives` Or (Not (Not (Ident "isLinux"))) (Ident "isDarwin")
        parse expr "!a && b || c" `gives` Or (And (Not (Ident "a")) (Ident "b")) (Ident "c")
        parse expr "a && b || c" `gives` Or (And (Ident "a") (Ident "b")) (Ident "c")
        parse expr "a || b && c" `gives` Or (Ident "a") (And (Ident "b") (Ident "c"))
        parse expr "(a -> b) -> c" `gives` Implies (Implies (Ident "a") (Ident "b")) (Ident "c")
      it "parses simple lambda expressions" $ do
        parse expr "x: {}" `gives` Fun (Ident "x") (AttrSet False [])
        parse expr "x: y: rec{}" `gives` Fun (Ident "x") (Fun (Ident "y") (AttrSet True []))
      it "parses attribute set patterns" $ do
        parse expr "{}: {}" `gives` Fun (AttrSet False []) (AttrSet False [])
        parse expr "{a ? b, c}: {}" `gives` Fun (AttrSetP Nothing [("a",Just (Ident "b")),("c",Nothing)]) (AttrSet False [])
        parse expr "{ id = x: x; }" `gives` AttrSet False [Assign (SIdent ["id"]) (Fun (Ident "x") (Ident "x"))]
        parse expr "{ a?null, b }: rec {}" `gives` Fun (AttrSetP Nothing [("a",Just (Ident "null")),("b",Nothing)]) (AttrSet True [])
        parse expr "{ a?c.d }: {}" `gives` Fun (AttrSetP Nothing [("a",Just (Deref (Ident "c") (Ident "d")))]) (AttrSet False [])
        parse expr "{ a?c.d, ... }: {}" `gives` Fun (AttrSetP Nothing [("a",Just (Deref (Ident "c") (Ident "d"))),("...",Nothing)]) (AttrSet False [])
        parse expr "e@{ a?c.d, ... }: {}" `gives` Fun (AttrSetP (Just "e") [("a",Just (Deref (Ident "c") (Ident "d"))),("...",Nothing)]) (AttrSet False [])
      it "ignores leading/trailing whitespace" $ do
        parse expr "   {}" `gives` AttrSet False []
        parse expr "{}   " `gives` AttrSet False []
        parse expr " { } " `gives` AttrSet False []
      it "ignores comments" $
        parse expr "# foo\n/* bar \n */ { /* bla */ }" `gives` AttrSet False []
      it "parses function application" $ do
        parse expr "a b c" `gives` Apply (Apply (Ident "a") (Ident "b")) (Ident "c")
        parse expr "a.b c" `gives` Apply (Deref (Ident "a") (Ident "b")) (Ident "c")
        parse expr "a{b=c.d;}" `gives` Apply (Ident "a") (AttrSet False [Assign (SIdent ["b"]) (Deref (Ident "c") (Ident "d"))])
      it "parses import statements" $ do
        parse expr "(import ../some/function.nix) c" `gives` Apply (Import (Lit "../some/function.nix")) (Ident "c")
        parse expr "let x = import ../some/function.nix; in x" `gives` Let [("x",Import (Lit "../some/function.nix"))] (Ident "x")
      it "parses if-then-else statements" $
        parse expr "if a b then c { inherit d; } else e" `gives` IfThenElse (Apply (Ident "a") (Ident "b")) (Apply (Ident "c") (AttrSet False [Inherit (SIdent []) ["d"]])) (Ident "e")
      it "parses with statements" $
        parse expr "with a; a" `gives` Apply (With (Ident "a")) (Ident "a")

    describe "run" $ do
      it "can evaluate simple data types" $ do
        run "null" `gives` Null
        run "true" `gives` Boolean True
        run "false" `gives` Boolean False
        run "\"null\"" `gives` Lit "null"
        run "123" `gives` Lit "123"
        run "http://example.org" `gives` Lit "http://example.org"
      it "can evaluate hand-picked Nix expressions" $ do
        -- run "rec { y = \"bar\"; f = x: \"foo\" + x; v = f y; }.v" `gives` Lit "foobar"
        -- run "{ a.a.a=1; a.a.b=2; a.b=3; }" `gives` AttrSetV (fromList [("a",AttrSetV (fromList [("a",AttrSetV (fromList [("a",StrV "1"),("b",StrV "2")])),("b",StrV "3")]))])
        -- run "{ a.a.a=1; a.a.b=2; a.b=3; }.a.a.a" `gives` Lit "1"
        run "bla or false" `gives` Boolean False
        run "let a = \"foo\"; b = a; f = x: x+\"bar\"; in f b" `gives` Lit "foobar"
        run "let a = { b = \"claus\"; };  b = \"bar\"; in { a = \"foo\"; inherit b; c = a.b; }" `gives` AttrSet False [Assign (SIdent ["a"]) (Lit "foo"),Assign (SIdent ["b"]) (Lit "bar"),Assign (SIdent ["c"]) (Lit "claus")]
        run "let a = { b = \"claus\"; };  b = \"bar\"; in { a = \"foo\"; inherit b; c = a.b; }.c" `gives` Lit "claus"
