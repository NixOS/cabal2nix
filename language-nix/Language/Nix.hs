{- |
   Module      :  Language.Nix
   Copyright   :  (c) 2013 Peter Simons
   License     :  BSD3
   Maintainer  :  simons@cryp.to
 -}

module Language.Nix
  (
    -- * Evaluating the Nix Language
    run, runEval, eval, builtins,

    -- * Running the Parser
    parseNixFile, parseNix, parse, parse', ParseError,

    -- * Nix Language AST
    Expr(..), ScopedIdent(..), Attr(..), genIdentifier,

    -- * Nix Language Parsers
    expr, listExpr, term, operatorTable, listOperatorTable, identifier, literal,
    nixString, literalURI, attrSet, scopedIdentifier, attribute, list, letExpr,
    attrSetPattern,

    -- * Parsec Language Specification
    TokenParser, LanguageDef, NixParser, NixOperator, nixLanguage, nixLexer,
    symbol, reserved, reservedOp, lexeme, parens, braces, brackets, natural,
    assign, semi, dot, commaSep1, whitespace,
  )
  where

import Prelude hiding ( lookup )
import Data.Functor.Identity
import Control.Applicative ( (<$>), (<*>), (<$), (<*), (*>) )
import Text.Parsec hiding ( parse )
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Language as Parsec
import qualified Text.Parsec.Token as Parsec
import Text.Parsec.Expr
import Test.QuickCheck

import Text.Show.Functions ( )
import Control.Monad.Reader
import qualified Control.Monad.Error as ErrT
import Control.Monad.Error hiding ( Error )
import qualified Data.Map as Map ( )
import Data.Map hiding ( map, foldr )

-- import Debug.Trace
trace :: a -> b -> b
trace _ b = b

----- Nix Language Definition for Parsec --------------------------------------

type TokenParser = Parsec.GenTokenParser String () Identity
type LanguageDef = Parsec.GenLanguageDef String () Identity
type NixParser a = ParsecT String () Identity a
type NixOperator = Operator String () Identity Expr

nixLanguage :: LanguageDef
nixLanguage = Parsec.emptyDef
  { Parsec.commentStart    = "/*"
  , Parsec.commentEnd      = "*/"
  , Parsec.commentLine     = "#"
  , Parsec.nestedComments  = False
  , Parsec.identStart      = letter <|> oneOf "_"
  , Parsec.identLetter     = alphaNum <|> oneOf "-_"
  , Parsec.opStart         = Parsec.opLetter nixLanguage
  , Parsec.opLetter        = oneOf ".!{}[]+=?&|/:"
  , Parsec.reservedOpNames = [".","!","+","++","&&","||","?","=","//","==","!=",":"]
  , Parsec.reservedNames   = ["rec","let","in","import","with","inherit","assert","or","if","then","else"]
  , Parsec.caseSensitive   = True
  }

nixLexer :: TokenParser
nixLexer = Parsec.makeTokenParser nixLanguage

symbol :: String -> NixParser String
symbol = Parsec.symbol nixLexer

reserved :: String -> NixParser ()
reserved = Parsec.reserved nixLexer

reservedOp :: String -> NixParser ()
reservedOp = Parsec.reservedOp nixLexer

lexeme :: NixParser a -> NixParser a
lexeme = Parsec.lexeme nixLexer

parens :: NixParser a -> NixParser a
parens = Parsec.parens nixLexer

braces :: NixParser a -> NixParser a
braces = Parsec.braces nixLexer

brackets :: NixParser a -> NixParser a
brackets = Parsec.brackets nixLexer

natural :: NixParser String
natural = show <$> Parsec.natural nixLexer

assign :: NixParser String
assign = symbol "="

semi :: NixParser String
semi = Parsec.semi nixLexer

dot :: NixParser String
dot = Parsec.dot nixLexer

commaSep1 :: NixParser a -> NixParser [a]
commaSep1 = Parsec.commaSep1 nixLexer

whitespace :: NixParser ()
whitespace = Parsec.whiteSpace nixLexer

----- Nix Expressions ---------------------------------------------------------

newtype ScopedIdent = SIdent [String]
  deriving (Read, Show, Eq)

data Attr = Assign ScopedIdent Expr
          | Inherit ScopedIdent [String]
  deriving (Read, Show, Eq)

genIdentifier :: Gen String
genIdentifier = ((:) <$> elements firstChar <*> listOf (elements identChar)) `suchThat` (`notElem` Parsec.reservedNames nixLanguage)
  where firstChar = ['a'..'z'] ++ ['A'..'Z'] ++ "_"
        identChar = firstChar ++ ['0'..'9'] ++ "-"

instance Arbitrary ScopedIdent where
  arbitrary = SIdent <$> listOf1 genIdentifier

data Expr = Null
          | Lit String
          | Ident String
          | Boolean Bool
          | AttrSet Bool [Attr]
          | AttrSetP (Maybe String) [(String, Maybe Expr)]
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
          | Fun Expr Expr
          | Let [Attr] Expr
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
              , try attrSetPattern
              , attrSet
              , letExpr
              , reserved "import" >> Import <$> expr
              , reserved "with" >> With <$> expr <* semi
              , reserved "assert" >> Assert <$> expr <* semi
              , IfThenElse <$> (reserved "if" *> expr) <*> (reserved "then" *> expr) <*> (reserved "else" *> expr)
              , try literal
              , identifier
              ]

operatorTable :: [[NixOperator]]
operatorTable = x1 : x2 : [ Infix (Apply <$ whitespace) AssocLeft ] : xs
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
                    , [ binary ":" Fun AssocRight ]
                    ]
  where
    binary :: String -> (Expr -> Expr -> Expr) -> Assoc -> NixOperator
    binary op fun = Infix (fun <$ reservedOp op)

    prefix :: String -> (Expr -> Expr) -> NixOperator
    prefix op fun = Prefix (fun <$ reservedOp op)

identifier :: NixParser Expr
identifier = Ident <$> Parsec.identifier nixLexer

literal :: NixParser Expr
literal = Lit <$> (stringLiteral <|> nixString <|> natural <|> literalURI)

stringLiteral :: NixParser String
stringLiteral = lexeme $ between (string "\"") (string "\"") (concat <$> many stringChar)
  where
    stringChar :: NixParser String
    stringChar = choice [ many1 (noneOf "$\\\"")
                        , try $ char '$' >> braces expr >> return ""
                        , return <$> char '$'
                        , char '\\' >> anyChar >>= \c -> return ['\\',c]
                        ]

nixString :: NixParser String
nixString = lexeme $ between (string "''") (string "''") (concat <$> many stringChar)
  where
    stringChar :: NixParser String
    stringChar = choice [ many1 (noneOf "'")
                        , try $ (return <$> char '\'') <* notFollowedBy (char '\'')
                        , try $ string "''" >> string "${"
                        ]

literalURI :: NixParser String
literalURI = lexeme $ try absoluteURI <|> relativeURI

absoluteURI :: NixParser String
absoluteURI = (++) <$> scheme <*> ((:) <$> char ':' <*> (hierPart <|> opaquePart))

relativeURI :: NixParser String
relativeURI = (++) <$> (absPath <|> relPath) <*> option "" (char '?' >> query)

absPath :: NixParser String
absPath = (:) <$> char '/' <*> pathSegments

authority :: NixParser String
authority = server <|> regName

domainlabel :: NixParser String
domainlabel = (:) <$> alphaNum <*> option "" ((++) <$> many (char '-') <*> domainlabel)

escapedChars :: NixParser Char
escapedChars = char '%' >> hexDigit >> hexDigit

hierPart :: NixParser String
hierPart = (++) <$> (try netPath <|> absPath) <*> option "" (char '?' >> query)

host :: NixParser String
host = hostname <|> ipv4address

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

pathSegments :: NixParser String
pathSegments = (++) <$> segment <*> (concat <$> many ((:) <$> char '/' <*> segment))

pchar :: NixParser Char
pchar = unreservedChars <|> escapedChars <|> oneOf ":@&=+$,"

port :: NixParser String
port = many1 digit

query :: NixParser String
query = many uric

regName :: NixParser String
regName = many1 (unreservedChars <|> escapedChars <|> oneOf "$,:@&=+") -- Note that ';' has been removed here!

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
toplabel = letter <|> (letter >> many (alphaNum <|> char '-') >> alphaNum)

unreservedChars :: NixParser Char
unreservedChars = alphaNum <|> markChars

uric :: NixParser Char
uric = reservedChars <|> unreservedChars <|> escapedChars

uricNoSlash :: NixParser Char
uricNoSlash = unreservedChars <|> escapedChars <|> oneOf ";?:@&=+$,"

userinfo :: NixParser String
userinfo = many (unreservedChars <|> escapedChars <|> oneOf ";:&=+$,")

attrSet :: NixParser Expr
attrSet = AttrSet <$> option False (True <$ reserved "rec") <*> braces (attribute `endBy` semi)

scopedIdentifier :: NixParser ScopedIdent
scopedIdentifier = SIdent <$> sepBy1 (Parsec.identifier nixLexer) dot

attribute :: NixParser Attr
attribute =  (Assign <$> (SIdent . return <$> stringLiteral <|> scopedIdentifier) <* assign <*> expr)
         <|> (Inherit <$> (symbol "inherit" *> option (SIdent []) (parens scopedIdentifier)) <*> many1 (Parsec.identifier nixLexer))

list :: NixParser Expr
list = List <$> brackets (many listExpr)

attrSetPattern :: NixParser Expr
attrSetPattern = AttrSetP <$> optionMaybe atPattern <*> setPattern
  where
    atPattern  = Parsec.identifier nixLexer <* reserved "@"
    setPattern = braces $ commaSep1 $ (,) <$> Parsec.identifier nixLexer <*> optionMaybe (reservedOp "?" >> expr) <|> ellipsis
    ellipsis   = ("...",Nothing) <$ reserved "..."

letExpr :: NixParser Expr
letExpr = choice [ try $ Let <$> (reserved "let" *> try attribute `endBy1` semi) <*> (reserved "in" *> expr)
                 , (`Let` Ident "body") <$> (reserved "let" *> braces (try attribute `endBy1` semi))
                 ]

parseNixFile :: FilePath -> IO (Either ParseError Expr)
parseNixFile path = parse' (expr <* eof) path <$> readFile path

parseNix :: String -> Either ParseError Expr
parseNix = parse expr

parse' :: NixParser a -> SourceName -> String -> Either ParseError a
parse' = Parsec.parse

parse :: NixParser a -> String -> Either ParseError a
parse p = parse' (p <* eof) "<string>"

----- Nix Evaluation ----------------------------------------------------------

type VarName = String
type Env = Map VarName Expr

data Error = CannotCoerceToString Expr
           | CannotCoerceToBool Expr
           | TypeMismatch Expr
           | UndefinedVariable VarName
           | Unsupported Expr
           | Unstructured String
           | InvalidSyntax ParseError
  deriving (Show)

instance ErrT.Error Error where
  strMsg = Unstructured
  noMsg  = Unstructured "no error message available"

type Eval a = ErrorT Error (Reader Env) a

getEnv :: VarName -> Eval Expr
getEnv v = ask >>= maybe (throwError (UndefinedVariable v)) return . lookup v

onError :: Eval a -> (Error -> Bool, Eval a) -> Eval a
onError f (p,g) = catchError f (\e -> if p e then g else throwError e)

isUndefinedVariable :: Error -> Bool
isUndefinedVariable (UndefinedVariable _) = True
isUndefinedVariable _                     = False

isCoerceToString :: Error -> Bool
isCoerceToString (CannotCoerceToString _) = True
isCoerceToString _                        = False

isCoerceToBool :: Error -> Bool
isCoerceToBool (CannotCoerceToBool _) = True
isCoerceToBool _                      = False

evalBool :: Expr -> Eval Bool
evalBool e | trace ("evalBool " ++ show e) False = undefined
evalBool (Boolean x)    = return x
evalBool (Ident v)      = getEnv v >>= evalBool
evalBool (And x y)      = (&&) <$> evalBool x <*> evalBool y
evalBool (Or x y)       = (||) <$> evalBool x <*> evalBool y
evalBool (Not x)        = not <$> evalBool x
evalBool e@(Equal x y)  = ((==) <$> evalString  x <*> evalString  y)
                            `onError` (isCoerceToString, (==) <$> evalBool x <*> evalBool y)
                            `onError` (isCoerceToBool, throwError (TypeMismatch e))
evalBool e              = throwError (CannotCoerceToBool e)

evalString :: Expr -> Eval String
evalString e | trace ("evalString " ++ show e) False = undefined
evalString (Lit x)      = return x
evalString (Append x y) = (++) <$> evalString x <*> evalString y
evalString (Ident v)    = getEnv v >>= evalString
evalString e            = throwError (CannotCoerceToString e)

evalAttribute :: Attr -> Eval [(VarName,Expr)]
evalAttribute (Assign (SIdent [k]) v)  = (return . (,) k) <$> eval v
evalAttribute (Inherit (SIdent []) vs) = sequence [ (,) v <$> getEnv v | v <- vs ]
evalAttribute e                        = throwError (Unsupported (AttrSet False [e]))

attrSetToEnv :: Attr -> Eval [(VarName,Expr)]
attrSetToEnv (Assign (SIdent [k]) v)  = return [(k,v)]
attrSetToEnv (Inherit (SIdent []) vs) = sequence [ (,) v <$> getEnv v | v <- vs ]
attrSetToEnv e                        = throwError (Unsupported (AttrSet True [e]))

eval :: Expr -> Eval Expr
eval e | trace ("eval " ++ show e) False = undefined
eval Null                                       = return Null
eval e@(Lit _)                                  = return e
eval e@(Boolean _)                              = return e
eval (Ident v)                                  = getEnv v >>= eval
eval e@(Append _ _)                             = Lit <$> evalString e
eval e@(And _ _)                                = Boolean <$> evalBool e
eval e@(Or _ _)                                 = Boolean <$> evalBool e
eval e@(Not _)                                  = Boolean <$> evalBool e
eval e@(Equal _ _)                              = Boolean <$> evalBool e
eval e@(Inequal _ _)                            = Boolean <$> evalBool e
eval (IfThenElse b x y)                         = evalBool b >>= \b' -> eval (if b' then x else y)
eval (DefAttr x y)                              = eval x `onError` (isUndefinedVariable, eval y)
eval (Let as e)                                 = concat <$> mapM attrSetToEnv as >>= \env -> trace ("add to env: " ++ show env) $ local (union (fromList env)) (eval e)
eval (Apply (Fun (Ident v) x) y)                = trace "foo" $ eval y >>= \y' -> local (insert v y') (eval x)
eval (Apply (Ident v) y)                        = trace "yo" $ getEnv v >>= \x' -> eval (Apply x' y)
eval (Apply x@(Apply _ _) y)                    = trace "yo" $ eval x >>= \x' -> eval (Apply x' y)
eval (AttrSet False as)                         = (AttrSet False . map (\(k,v) -> Assign (SIdent [k]) v) . concat) <$> mapM evalAttribute as
eval (AttrSet True as)                          = concat <$> mapM attrSetToEnv as >>= \as' -> trace ("add to env: " ++ show as') $ local (union (fromList as')) (eval (AttrSet False as))
eval (Deref (Ident v) y)                        = getEnv v >>= \v' -> eval (Deref v' y)
eval (Deref (AttrSet False as) y@(Ident _))     = concat <$> mapM evalAttribute as >>= \as' -> trace ("add to env: " ++ show as') $ local (\env -> foldr (uncurry insert) env as') (eval y)
eval (Deref (AttrSet True as) y@(Ident _))      = concat <$> mapM attrSetToEnv as >>= \as' -> trace ("add to env: " ++ show as') $ local (\env -> foldr (uncurry insert) env as') (eval y)
eval e@(Deref _ _)                              = throwError (TypeMismatch e)
eval e                                          = throwError (Unsupported e)

--
-- eval (Apply (Lambda v x) y)     = eval y >>= \y' -> trace ("add to env: " ++ show (v,y')) $ local ((v,y'):) (eval x)
-- eval (Apply x@(V _) y)          = eval x >>= \x' -> eval (Apply x' y)
-- eval (Apply x@(Apply _ _) y)    = eval x >>= \x' -> eval (Apply x' y)
-- eval (Let env e)                = trace ("add to env: " ++ show env) $ local (env++) (eval e)
-- eval e@(Lambda _ _)             = return e
-- eval e                          = throwError (Unsupported e)


-- coerceDict :: Value -> Dict
-- coerceDict (AttrSetV e) = e
-- coerceDict e            = error ("cannot coerce expression to attribute set: " ++ show e)
--
-- coerceFun :: Value -> (Value -> Value)
-- coerceFun (FunV f) = f
-- coerceFun e        = error ("cannot coerce expression to function: " ++ show e)
--
-- coerceStr :: Value -> String
-- coerceStr (StrV x) = x
-- coerceStr e        = error ("cannot coerce expression to string: " ++ show e)
--
-- -- getScopedVar :: [String] -> Eval Value
-- -- getScopedVar   []   = fail "invalid empty scoped variable"
-- -- getScopedVar (k:[]) = getEnv k
-- -- getScopedVar (k:ks) = getEnv k >>= \e -> local (union (coerceDict e)) (getScopedVar ks)
--
-- -- evalAttr :: Attr -> Eval Dict
-- -- evalAttr (Inherit (SIdent k) is)    = fromList <$> forM is (\i -> (,) i <$> getScopedVar (k++[i]))
-- -- evalAttr (Assign (SIdent   []) _)   = fail "invalid empty scoped identifier in assignment"
-- -- evalAttr (Assign (SIdent (k:[])) e) = singleton k <$> eval e
-- -- evalAttr (Assign (SIdent (k:ks)) e) = (singleton k . AttrSetV) <$> evalAttr (Assign (SIdent ks) e)
--
-- simplifyAttr :: Attr -> Map String Expr
-- simplifyAttr (Inherit (SIdent _) [])    = error "invalid empty inherit statement"
-- simplifyAttr (Inherit (SIdent k) is)    = unions [ singleton i (foldl1 Deref (map Ident (k++[i]))) | i <- is]
-- simplifyAttr (Assign (SIdent   []) _)   = error "invalid empty scoped identifier in assignment"
-- simplifyAttr (Assign (SIdent (k:[])) e) = singleton k e
-- simplifyAttr (Assign (SIdent (k:ks)) e) = singleton k (AttrSet False [Assign (SIdent ks) e])
--
-- evalAttr' :: (String, Expr) -> Eval Dict
-- evalAttr' (k, e) = singleton k <$> eval e
--
-- evalDict :: Map String Expr -> Eval Dict
-- evalDict as = unionsWith mergeDicts <$> mapM evalAttr' (assocs as)
--
-- -- -- (Inherit (SIdent k) is)    = fromList <$> forM is (\i -> (,) i <$> getScopedVar (k++[i]))
-- -- evalAttr' (Assign (SIdent   []) _)   = fail "invalid empty scoped identifier in assignment"
-- -- evalAttr' (Assign (SIdent (k:[])) e) = singleton k <$> eval e
-- -- evalAttr' (Assign (SIdent (k:ks)) e) = (singleton k . AttrSetV) <$> evalAttr (Assign (SIdent ks) e)
--
-- eval :: Expr -> Eval Value
-- eval e | trace ("eval: " ++ show e) False = undefined
-- eval (Lit v)                    = return (StrV v)
-- eval (Ident v)                  = getEnv v
-- eval (AttrSet False as)         = AttrSetV . unionsWith mergeDicts <$> mapM (evalDict . simplifyAttr) as
--
-- eval (AttrSet True as)          = do
--   env <- ask
--   let e :: Map String Expr
--       e = unionsWith mergeAttrSets (map simplifyAttr as)
--   return (AttrSetV (resolve env e))
--
-- -- mdo { r@(AttrSetV d) <- local (`union` d) (eval (AttrSet False as)); return r }
--
-- -- eval (AttrSet False as)         = AttrSetV . unionsWith mergeDicts <$> mapM evalAttr as
-- -- eval (AttrSet True as)          = mdo { r@(AttrSetV d) <- local (`union` d) (eval (AttrSet False as)); return r }
-- eval (Fun (Ident x) y)          = do { env <- ask; return (FunV (\v -> runEval' (eval y) (insert x v env))) }
-- eval (Apply x y)                = coerceFun <$> eval x <*> eval y
-- eval (Append x y)               = StrV <$> ((++) <$> (coerceStr <$> eval x) <*> (coerceStr <$> eval y))
-- eval (Deref x (Ident y))        = coerceDict <$> eval x >>= \x' -> local (const x') (getEnv y)
-- -- default catch-all to report the un-expected expression
-- eval e                          = fail ("unsupported: " ++ show e)
--
-- mergeDicts :: Value -> Value -> Value
-- mergeDicts x y = AttrSetV (unionWith mergeDicts (coerceDict x) (coerceDict y))
--
-- mergeAttrSets :: Expr -> Expr -> Expr
-- mergeAttrSets (AttrSet False x) (AttrSet False y) = AttrSet False (x++y)
-- mergeAttrSets x y = error ("mergeAttrSets: cannot merge expressions " ++ show x ++ " and " ++ show y)

run :: String -> Either Error Expr
run = either (Left . InvalidSyntax) (\e -> runEval (eval e) builtins) . parseNix

runEval :: Eval a -> Env -> Either Error a
runEval = runReader . runErrorT

builtins :: Env
builtins = fromList
           [ ("true", Boolean True)
           , ("false", Boolean False)
           , ("null", Null)
           ]
