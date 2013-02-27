module Main where

import Data.Functor.Identity
import Control.Applicative ( (<$>), (<*>), (<$), (<*), (*>) )
import Text.Parsec hiding ( parse )
import qualified Text.Parsec as Parse
import qualified Text.Parsec.Language as Parse ( emptyDef )
import qualified Text.Parsec.Token as Parse
import Text.Parsec.Expr
import Text.PrettyPrint.Leijen ( Pretty(..) )
import qualified Text.PrettyPrint.Leijen as Pretty
import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.DocTest

----- Nix Language Definition for Parsec --------------------------------------

type TokenParser st = Parse.GenTokenParser String st Identity
type LanguageDef st = Parse.GenLanguageDef String st Identity
type NixParser st a = ParsecT String st Identity a
type NixOperator st = Operator String st Identity Expr

nixLanguage :: LanguageDef st
nixLanguage = Parse.emptyDef
  { Parse.commentStart    = "/*"
  , Parse.commentEnd      = "*/"
  , Parse.commentLine     = "#"
  , Parse.nestedComments  = False
  , Parse.identStart      = letter
  , Parse.identLetter     = alphaNum <|> oneOf "-_"
  , Parse.opStart         = Parse.opLetter nixLanguage
  , Parse.opLetter        = oneOf ".!:{}[]+=?&|/"
  , Parse.reservedOpNames = [".","!","+","++","&&","||","?","=",":","//","==","!="]
  , Parse.reservedNames   = ["rec","let","in","import","with","inherit","or"]
  , Parse.caseSensitive   = True
  }

nixLexer :: TokenParser st
nixLexer = Parse.makeTokenParser nixLanguage

symbol :: String -> NixParser st String
symbol = Parse.symbol nixLexer

reserved :: String -> NixParser st ()
reserved = Parse.reserved nixLexer

reservedOp :: String -> NixParser st ()
reservedOp = Parse.reservedOp nixLexer

lexeme :: NixParser st a -> NixParser st a
lexeme = Parse.lexeme nixLexer

parens :: NixParser st a -> NixParser st a
parens = Parse.parens nixLexer

braces :: NixParser st a -> NixParser st a
braces = Parse.braces nixLexer

brackets :: NixParser st a -> NixParser st a
brackets = Parse.brackets nixLexer

comma :: NixParser st String
comma = Parse.comma nixLexer

assign :: NixParser st String
assign = symbol "="

semi :: NixParser st String
semi = Parse.semi nixLexer

dot :: NixParser st String
dot = Parse.dot nixLexer

commaSep :: NixParser st a -> NixParser st [a]
commaSep = Parse.commaSep nixLexer

colon :: NixParser st String
colon = Parse.colon nixLexer

whitespace :: NixParser st ()
whitespace = Parse.whiteSpace nixLexer

----- Nix Expressions ---------------------------------------------------------

newtype ScopedIdent = SIdent [String]
  deriving (Read, Show, Eq)

genIdentifier :: Gen String
genIdentifier = (:) <$> elements alpha <*> listOf (elements tok)
  where alpha = ['a'..'z'] ++ ['A'..'Z']
        tok   = alpha ++ ['0'..'9'] ++ "-_"

instance Arbitrary ScopedIdent where
  arbitrary = SIdent <$> listOf1 genIdentifier

instance Pretty ScopedIdent where
  pretty (SIdent xs) = Pretty.hcat $ Pretty.punctuate Pretty.dot (map Pretty.text xs)

data Binding = Lambda String
             | RecB [(String, Maybe Expr)]
  deriving (Read, Show, Eq)

data Expr = Lit String
          | Ident String
          | Record Bool [(ScopedIdent,Expr)]
          | List [Expr]
          | Deref Expr Expr
          | HasAttr Expr Expr
          | Concat Expr Expr
          | Append Expr Expr
          | Not Expr
          | Union Expr Expr
          | Equal Expr Expr
          | Inequal Expr Expr
          | And Expr Expr
          | Or Expr Expr
          | Implies Expr Expr
          | Fun Binding Expr
          | Let [(String,Expr)] Expr
          | Apply Expr Expr
  deriving (Read, Show, Eq)

expr :: NixParser st Expr
expr = whitespace >> buildExpressionParser operatorTable term

listExpr :: NixParser st Expr
listExpr = whitespace >> buildExpressionParser listOperatorTable term

term :: NixParser st Expr
term = choice [ parens expr
              , stringLiteral
              , list
              , try function
              , try letExpr
              , record
              , identifier
              ]

operatorTable :: [[NixOperator st]]
operatorTable = x : [ Infix (Apply <$ whitespace) AssocRight ] : xs
  where (x:xs) = listOperatorTable

listOperatorTable :: [[NixOperator st]]
listOperatorTable = [ [ binary "." Deref AssocLeft ]
                {-  , [ Infix (Apply <$ whitespace) AssocRight ] -}
                    , [ binary "?" HasAttr AssocNone ]
                    , [ binary "++" Concat AssocRight ]
                    , [ binary "+" Append AssocLeft ]
                    , [ prefix "!" Not ]
                    , [ binary "//" Union AssocRight ]
                    , [ binary "==" Equal AssocNone ]
                    , [ binary "!=" Inequal AssocNone ]
                    , [ binary "&&" And AssocLeft ]
                    , [ binary "||" Or AssocLeft ]
                    , [ binary "->" Implies AssocNone ]
                    ]
  where
    binary :: String -> (Expr -> Expr -> Expr) -> Assoc -> NixOperator st
    binary op fun = Infix (fun <$ reservedOp op)

    prefix :: String -> (Expr -> Expr) -> NixOperator st
    prefix op fun = Prefix (fun <$ reservedOp op)

identifier :: NixParser st Expr
identifier = Ident <$> Parse.identifier nixLexer

stringLiteral :: NixParser st Expr
stringLiteral = Lit <$> Parse.stringLiteral nixLexer

record :: NixParser st Expr
record = Record <$> option False (True <$ reserved "rec") <*> braces (many recordAssignment)

scopedIdentifier :: NixParser st ScopedIdent
scopedIdentifier = SIdent <$> sepBy1 (Parse.identifier nixLexer) dot

recordAssignment :: NixParser st (ScopedIdent, Expr)
recordAssignment = (,) <$> scopedIdentifier <* assign <*> expr <* semi

list :: NixParser st Expr
list = List <$> brackets (many listExpr)

function :: NixParser st Expr
function = Fun <$> (recordBinding <|> simpleBinding) <* colon <*> expr

simpleBinding :: NixParser st Binding
simpleBinding = Lambda <$> Parse.identifier nixLexer

recordBinding :: NixParser st Binding
recordBinding = fmap RecB $ braces $ commaSep $ (,) <$> Parse.identifier nixLexer <*> optionMaybe (reservedOp "?" >> expr)

letExpr :: NixParser st Expr
letExpr = Let <$> (reserved "let" *> many1 letAssignment) <*> (reserved "in" *> expr)

letAssignment :: NixParser st (String, Expr)
letAssignment = (,) <$> Parse.identifier nixLexer <* assign <*> expr <* semi

----- test program ------------------------------------------------------------

gives :: (Eq a, Show a) => Either ParseError a -> a -> Expectation
gives x y = x `shouldSatisfy` either (const False) (==y)

parse :: NixParser () a -> String -> Either ParseError a
parse p input = Parse.parse (p <* eof) (show input) input

parseFail :: Show a => NixParser () a -> String -> Expectation
parseFail p input = parse p input `shouldSatisfy` either (const True) (const False)

main :: IO ()
main = do
  doctest [ "NixParser.hs" ]
  hspec $ do
    describe "identifier" $ do
      it "parses hand-picked sample inputs" $ do
        parse identifier "abc" `gives` Ident "abc"
        parse identifier "abc  " `gives` Ident "abc"
      prop "parses all randomly generated samples" $
        forAll genIdentifier $ \input -> either (const False) (Ident input ==) (parse identifier input)
      it "does not swallow leading whitespace" $
        parseFail identifier " abc"
      it "does not parse a de-referencing expression" $
        parseFail identifier "abc.def"
      it "does not accept reserved words" $
        mapM_ (parseFail identifier) (Parse.reservedNames nixLanguage)
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
        mapM_ (parseFail scopedIdentifier) (Parse.reservedNames nixLanguage)
      it "accepts identifiers that are a prefix of a reserved word" $
        parse scopedIdentifier "lett" `gives` SIdent ["lett"]

    describe "record" $ do
      it "parses an empty record" $ do
        parse record "{}" `gives` Record False []
        parse record "rec {}" `gives` Record True []
      it "parses hand-picked sample inputs" $ do
        parse record "{ a = b; }" `gives` Record False [(SIdent ["a"],Ident "b")]
        parse record "{ a = b.c; }" `gives` Record False [(SIdent ["a"], Deref (Ident "b") (Ident "c"))]
        parse record "{ a = \"b\"; }" `gives` Record False [(SIdent ["a"],Lit "b")]
      it "parses records as values of records" $
        parse record "{ a = { b = c; }; }" `gives` Record False [(SIdent ["a"],Record False [(SIdent ["b"],Ident "c")])]
      it "expects assignments to terminated by a semicolon" $
        parseFail record "{ a = b }"
      it "ignores comments" $
        parse record "{ /* foo */ a = /* bar */ b; # foobar\n }" `gives` Record False [(SIdent ["a"],Ident "b")]
      it "parses recursive records" $
        parse record "rec { a = b; b = a; }" `gives` Record True [(SIdent ["a"],Ident "b"),(SIdent ["b"],Ident "a")]

    describe "list" $ do
      it "parses an empty list" $
        parse list "[]" `gives` List []
      it "parses hand-picked sample inputs" $ do
        parse list "[ a b c ]" `gives` List [Ident "a",Ident "b",Ident "c"]
        parse list "[ \"b\" { a = [\"c\"]; } d ]" `gives` List [Lit "b",Record False [(SIdent ["a"],List [Lit "c"])],Ident "d"]
        parse list "[ (a b) c ]" `gives` List [Apply (Ident "a") (Ident "b"),Ident "c"]

    describe "reserved" $ do
      it "parses a specific reserved name" $ do
         parse (reserved "let") "let" `gives` ()
         parse (reserved "in") "in" `gives` ()
         parse (reserved "rec") "rec" `gives` ()
         parseFail (reserved "rec") "let"
      it "recognizes if the keyword is actually just a prefix of the input string" $
         parseFail (reserved "in") "input"

    describe "function" $ do
      it "parses simple lambda expressions" $ do
        parse function "x: {}" `gives` Fun (Lambda "x") (Record False [])
        parse function "x: y: rec{}" `gives` Fun (Lambda "x") (Fun (Lambda "y") (Record True []))
      it "parses record bindings" $ do
        parse function "{}: {}" `gives` Fun (RecB []) (Record False [])
        parse function "{a ? b, c}: {}" `gives` Fun (RecB [("a",Just (Ident "b")),("c",Nothing)]) (Record False [])

    describe "expr" $ do
      it "parses an empty record" $ do
        parse expr "{}" `gives` Record False []
        parse expr "rec {}" `gives` Record True []
      it "parses an empty list" $
        parse expr "[]" `gives` List []
      it "parses a de-referencing expression" $ do
        parse expr "abc.def" `gives` Deref (Ident "abc") (Ident "def")
        parse expr "a.b.c" `gives` Deref (Deref (Ident "a") (Ident "b")) (Ident "c")
      it "parses recursive records" $
        parse expr "rec { id = x: x; }" `gives` Record True [(SIdent ["id"], Fun (Lambda "x") (Ident "x"))]
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
      it "parses functions" $ do
        parse expr "{ id = x: x; }" `gives` Record False [(SIdent ["id"], Fun (Lambda "x") (Ident "x"))]
        parse expr "{}: {}" `gives` Fun (RecB []) (Record False [])
        parse expr "{}: rec {}" `gives` Fun (RecB []) (Record True [])
        parse expr "{ a?null, b }: {}" `gives` Fun (RecB [("a",Just (Ident "null")),("b",Nothing)]) (Record False [])
        parse expr "{ a?c.d }: {}" `gives` Fun (RecB [("a",Just (Deref (Ident "c") (Ident "d")))]) (Record False [])
      it "ignores leading/trailing whitespace" $ do
        parse expr "   {}" `gives` Record False []
        parse expr "{}   " `gives` Record False []
        parse expr " { } " `gives` Record False []
      it "ignores comments" $
        parse expr "# foo\n/* bar \n */ { /* bla */ }" `gives` Record False []
      it "parses function application" $ do
        parse expr "a b c" `gives` Apply (Ident "a") (Apply (Ident "b") (Ident "c"))
        parse expr "a.b c" `gives` Apply (Deref (Ident "a") (Ident "b")) (Ident "c")
        parse expr "a{b=c.d;}" `gives` Apply (Ident "a") (Record False [(SIdent ["b"],Deref (Ident "c") (Ident "d"))])

parseNixFile :: FilePath -> IO (Either ParseError Expr)
parseNixFile path = parse expr <$> readFile path

allPackages, top :: IO (Either ParseError Expr)
allPackages = parseNixFile "/home/simons/.nix-defexpr/pkgs/top-level/all-packages.nix"
top = parseNixFile "/home/simons/.nix-defexpr/default.nix"
