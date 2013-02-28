{- |
   Module      :  Main
   Copyright   :  (c) 2013 Peter Simons
   License     :  BSD3
   Maintainer  :  simons@cryp.to
 -}

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
import Test.HUnit.Base ( assertFailure, assertEqual )
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.DocTest
import Data.List

----- Nix Language Definition for Parsec --------------------------------------

type TokenParser = Parse.GenTokenParser String () Identity
type LanguageDef = Parse.GenLanguageDef String () Identity
type NixParser a = ParsecT String () Identity a
type NixOperator = Operator String () Identity Expr

nixLanguage :: LanguageDef
nixLanguage = Parse.emptyDef
  { Parse.commentStart    = "/*"
  , Parse.commentEnd      = "*/"
  , Parse.commentLine     = "#"
  , Parse.nestedComments  = False
  , Parse.identStart      = letter <|> oneOf "_"
  , Parse.identLetter     = alphaNum <|> oneOf "-_"
  , Parse.opStart         = Parse.opLetter nixLanguage
  , Parse.opLetter        = oneOf ".!{}[]+=?&|/"
  , Parse.reservedOpNames = [".","!","+","++","&&","||","?","=","//","==","!="]
  , Parse.reservedNames   = ["rec","let","in","import","with","inherit","assert","or","if","then","else"]
  , Parse.caseSensitive   = True
  }

nixLexer :: TokenParser
nixLexer = Parse.makeTokenParser nixLanguage

symbol :: String -> NixParser String
symbol = Parse.symbol nixLexer

reserved :: String -> NixParser ()
reserved = Parse.reserved nixLexer

reservedOp :: String -> NixParser ()
reservedOp = Parse.reservedOp nixLexer

lexeme :: NixParser a -> NixParser a
lexeme = Parse.lexeme nixLexer

parens :: NixParser a -> NixParser a
parens = Parse.parens nixLexer

braces :: NixParser a -> NixParser a
braces = Parse.braces nixLexer

brackets :: NixParser a -> NixParser a
brackets = Parse.brackets nixLexer

natural :: NixParser String
natural = show <$> Parse.natural nixLexer

assign :: NixParser String
assign = symbol "="

semi :: NixParser String
semi = Parse.semi nixLexer

dot :: NixParser String
dot = Parse.dot nixLexer

commaSep :: NixParser a -> NixParser [a]
commaSep = Parse.commaSep nixLexer

colon :: NixParser String
colon = Parse.colon nixLexer

whitespace :: NixParser ()
whitespace = Parse.whiteSpace nixLexer

----- Nix Expressions ---------------------------------------------------------

newtype ScopedIdent = SIdent [String]
  deriving (Read, Show, Eq)

data Attr = Assign ScopedIdent Expr
          | Inherit ScopedIdent [String]
  deriving (Read, Show, Eq)

genIdentifier :: Gen String
genIdentifier = ((:) <$> elements firstChar <*> listOf (elements identChar)) `suchThat` (`notElem` Parse.reservedNames nixLanguage)
  where firstChar = ['a'..'z'] ++ ['A'..'Z'] ++ "_"
        identChar = firstChar ++ ['0'..'9'] ++ "-"

instance Arbitrary ScopedIdent where
  arbitrary = SIdent <$> listOf1 genIdentifier

instance Pretty ScopedIdent where
  pretty (SIdent xs) = Pretty.hcat $ Pretty.punctuate Pretty.dot (map Pretty.text xs)

data Pattern = Lambda String
             | AttrSetP (Maybe String) [(String, Maybe Expr)]
  deriving (Read, Show, Eq)

data Expr = Lit String
          | Ident String
          | AttrSet Bool [Attr]
          | List [Expr]
          | Deref Expr Expr
          | HasAttr Expr Expr
          | DefAttr Expr Expr
          | Concat Expr Expr
          | Append Expr Expr
          | Not Expr
          | Union Expr Expr
          | Equal Expr Expr
          | Inequal Expr Expr
          | And Expr Expr
          | Or Expr Expr
          | Implies Expr Expr
          | Fun Pattern Expr
          | Let [(String,Expr)] Expr
          | Apply Expr Expr
          | Import Expr
          | With Expr
          | Assert Expr
          | IfThenElse Expr Expr Expr
  deriving (Read, Show, Eq)

expr :: NixParser Expr
expr = whitespace >> buildExpressionParser operatorTable term

listExpr :: NixParser Expr
listExpr = buildExpressionParser listOperatorTable term

term :: NixParser Expr
term = choice [ parens expr
              , list
              , try function
              , attrSet
              , try letExpr
              , try literal
              , try (reserved "import" >> Import <$> expr)
              , try (reserved "with" >> With <$> expr <* semi)
              , try (reserved "assert" >> Assert <$> expr <* semi)
              , try (IfThenElse <$> (reserved "if" *> expr) <*> (reserved "then" *> expr) <*> (reserved "else" *> expr))
              , identifier
              ]

operatorTable :: [[NixOperator]]
operatorTable = x1 : x2 : [ Infix (Apply <$ whitespace) AssocRight ] : xs
  where (x1:x2:xs) = listOperatorTable

listOperatorTable :: [[NixOperator]]
listOperatorTable = [ [ binary "." Deref AssocLeft ]
                    , [ binary "or" DefAttr AssocNone ]
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
    binary :: String -> (Expr -> Expr -> Expr) -> Assoc -> NixOperator
    binary op fun = Infix (fun <$ reservedOp op)

    prefix :: String -> (Expr -> Expr) -> NixOperator
    prefix op fun = Prefix (fun <$ reservedOp op)

identifier :: NixParser Expr
identifier = Ident <$> Parse.identifier nixLexer

literal :: NixParser Expr
literal = Lit <$> (Parse.stringLiteral nixLexer <|> nixString <|> natural <|> literalURL)

nixString :: NixParser String
nixString = lexeme $ between (string "''") (string "''") (many (try (noneOf "'" <|> (char '\'' >> notFollowedBy (char '\'') >> return '\''))))

literalURL :: NixParser String
literalURL = try absoluteURI <|> relativeURI

absoluteURI :: NixParser String
absoluteURI = lexeme $ (++) <$> scheme <*> ((:) <$> char ':' <*> (try hierPart <|> opaquePart))

relativeURI :: NixParser String
relativeURI = lexeme $ (++) <$> (absPath <|> relPath) <*> option "" (char '?' >> query)

absPath :: NixParser String
absPath = (:) <$> char '/' <*> pathSegments

authority :: NixParser String
authority = try server <|> regName

domainlabel :: NixParser String
domainlabel = (:) <$> alphaNum <*> option "" ((++) <$> many (char '-') <*> domainlabel)

escapedChars :: NixParser Char
escapedChars = char '%' >> hexDigit >> hexDigit

hierPart :: NixParser String
hierPart = (++) <$> (try netPath <|> absPath) <*> option "" (char '?' >> query)

host :: NixParser String
host = try hostname <|> ipv4address

hostname :: NixParser String
hostname = many (domainlabel >> char '.') >> toplabel >> option "" (string ".")

hostport :: NixParser String
hostport = (++) <$> host <*> option "" ((:) <$> char ':' <*> port)

ipv4address :: NixParser String
ipv4address = many1 digit >> char '.' >> many1 digit >> char '.' >> many1 digit >> char '.' >> many1 digit

markChars :: NixParser Char
markChars = oneOf "-_.!~*'" -- Note that "()" have been removed here!

netPath :: NixParser String
netPath = (++) <$> ((++) <$> string "//" <*> authority) <*> option "" absPath

opaquePart :: NixParser String
opaquePart = uricNoSlash >> many uric

param :: NixParser String
param = many pchar

pathSegments :: NixParser String
pathSegments = (++) <$> segment <*> (concat <$> many ((:) <$> char '/' <*> segment))

pchar :: NixParser Char
pchar = try unreservedChars <|> try escapedChars <|> oneOf ":@&=+$,"

port :: NixParser String
port = many1 digit

query :: NixParser String
query = many uric

regName :: NixParser String
regName = many1 (try unreservedChars <|> try escapedChars <|> oneOf "$,:@&=+") -- Note that ';' has been removed here!

relPath :: NixParser String
relPath = (++) <$> relSegment <*> absPath

relSegment :: NixParser String
relSegment = many1 (unreservedChars <|> escapedChars <|> oneOf "@&=+$,") -- Note that ';' has been removed here!

reservedChars :: NixParser Char
reservedChars = oneOf "/?:@&=+$," -- Note that ';' has been removed here!

scheme :: NixParser String
scheme = (:) <$> letter <*> many (alphaNum <|> oneOf "+-.")

segment :: NixParser String
segment = {- (++) <$> -} many pchar {- <*> (concat <$> many ((:) <$> char ';' <*> param)) -}

server :: NixParser String
server = option "" (option "" ((++) <$> userinfo <*> string "@") >> hostport)

toplabel :: NixParser Char
toplabel = try letter <|> (letter >> many (alphaNum <|> char '-') >> alphaNum)

unreservedChars :: NixParser Char
unreservedChars = try alphaNum <|> markChars

uric :: NixParser Char
uric = try reservedChars <|> try unreservedChars <|> escapedChars

uricNoSlash :: NixParser Char
uricNoSlash = try unreservedChars <|> try escapedChars <|> oneOf ";?:@&=+$,"

userinfo :: NixParser String
userinfo = many (try unreservedChars <|> try escapedChars <|> oneOf ";:&=+$,")

attrSet :: NixParser Expr
attrSet = AttrSet <$> option False (True <$ reserved "rec") <*> braces (many attribute)

scopedIdentifier :: NixParser ScopedIdent
scopedIdentifier = SIdent <$> sepBy1 (Parse.identifier nixLexer) dot

attribute :: NixParser Attr
attribute =  ((\x -> Assign (SIdent [x])) <$> (Parse.stringLiteral nixLexer) <* assign <*> expr <* semi)
         <|> (Assign <$> scopedIdentifier <* assign <*> expr <* semi)
         <|> (Inherit <$> (symbol "inherit" *> option (SIdent []) (parens scopedIdentifier)) <*> (many1 (Parse.identifier nixLexer) <* semi))

list :: NixParser Expr
list = List <$> brackets (many listExpr)

function :: NixParser Expr
function = Fun <$> (try attrSetPattern <|> simplePattern) <* colon <*> expr

simplePattern :: NixParser Pattern
simplePattern = Lambda <$> Parse.identifier nixLexer

attrSetPattern :: NixParser Pattern
attrSetPattern = AttrSetP <$> optionMaybe atPattern <*> attrSetPattern
  where
    atPattern      = Parse.identifier nixLexer <* reserved "@"
    attrSetPattern = braces $ commaSep $ (,) <$> Parse.identifier nixLexer <*> optionMaybe (reservedOp "?" >> expr) <|> ellipsis
    ellipsis       = ("...",Nothing) <$ reserved "..."

letExpr :: NixParser Expr
letExpr = Let <$> (reserved "let" *> many1 letAssignment) <*> (reserved "in" *> expr)

letAssignment :: NixParser (String, Expr)
letAssignment = (,) <$> Parse.identifier nixLexer <* assign <*> expr <* semi

----- test program ------------------------------------------------------------

gives :: (Eq a, Show a) => Either ParseError a -> a -> Expectation
gives x y = either (assertFailure . msg) (assertEqual "" y) x
  where msg z = "expected: " ++ show y ++ "\nbut got parser error: " ++ show z

parse' :: NixParser a -> String -> FilePath -> Either ParseError a
parse' = Parse.parse

parse :: NixParser a -> String -> Either ParseError a
parse p input = parse' (p <* eof) (show input) input

parseFail :: Show a => NixParser a -> String -> Expectation
parseFail p input = parse p input `shouldSatisfy` either (const True) (const False)

main :: IO ()
main = do
  doctest [ "NixParser.hs" ]
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

    describe "literal" $ do
      prop "parses all randomly generated literal strings" $
        \str -> either (const False) (Lit str ==) (parse literal (show str))
      prop "parses all randomly generated integers" $
        \n -> either (const False) (Lit (show (abs (n::Int))) ==) (parse literal (show (abs n)))
      it "parses paths" $
        parse literal "claus/ist/der/beste" `gives` Lit "claus/ist/der/beste"
      it "parses URIs" $
        parse literal "http://example.org" `gives` Lit "http://example.org"

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

    describe "function" $ do
      it "parses simple lambda expressions" $ do
        parse function "x: {}" `gives` Fun (Lambda "x") (AttrSet False [])
        parse function "x: y: rec{}" `gives` Fun (Lambda "x") (Fun (Lambda "y") (AttrSet True []))
      it "parses attribute set patterns" $ do
        parse function "{}: {}" `gives` Fun (AttrSetP Nothing []) (AttrSet False [])
        parse function "{a ? b, c}: {}" `gives` Fun (AttrSetP Nothing [("a",Just (Ident "b")),("c",Nothing)]) (AttrSet False [])

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
        parse expr "rec { id = x: x; }" `gives` AttrSet True [Assign (SIdent ["id"]) (Fun (Lambda "x") (Ident "x"))]
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
        parse expr "{ id = x: x; }" `gives` AttrSet False [Assign (SIdent ["id"]) (Fun (Lambda "x") (Ident "x"))]
        parse expr "{}: {}" `gives` Fun (AttrSetP Nothing []) (AttrSet False [])
        parse expr "{}: rec {}" `gives` Fun (AttrSetP Nothing []) (AttrSet True [])
        parse expr "{ a?null, b }: {}" `gives` Fun (AttrSetP Nothing [("a",Just (Ident "null")),("b",Nothing)]) (AttrSet False [])
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
        parse expr "a b c" `gives` Apply (Ident "a") (Apply (Ident "b") (Ident "c"))
        parse expr "a.b c" `gives` Apply (Deref (Ident "a") (Ident "b")) (Ident "c")
        parse expr "a{b=c.d;}" `gives` Apply (Ident "a") (AttrSet False [Assign (SIdent ["b"]) (Deref (Ident "c") (Ident "d"))])
      it "parses import statements" $ do
        parse expr "(import ../some/function.nix) c" `gives` Apply (Import (Lit "../some/function.nix")) (Ident "c")
        parse expr "let x = import ../some/function.nix; in x" `gives` Let [("x",Import (Lit "../some/function.nix"))] (Ident "x")
      it "parses if-then-else statements" $
        parse expr "if a b then c { inherit d; } else e" `gives` IfThenElse (Apply (Ident "a") (Ident "b")) (Apply (Ident "c") (AttrSet False [Inherit (SIdent []) ["d"]])) (Ident "e")
      it "parses with statements" $
        parse expr "with a; a" `gives` Apply (With (Ident "a")) (Ident "a")


parseNixFile :: FilePath -> IO (Either ParseError Expr)
parseNixFile path = parse' expr path <$> readFile path

allPackages, top, nixos :: IO (Either ParseError Expr)
allPackages = parseNixFile "/home/simons/.nix-defexpr/pkgs/top-level/all-packages.nix"
top = parseNixFile "/home/simons/.nix-defexpr/default.nix"
nixos = parseNixFile "/etc/nixos/configuration.nix"
