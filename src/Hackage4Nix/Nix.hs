module Hackage4Nix.Nix where

import Hackage4Nix.Nix.Lex
import Hackage4Nix.Nix.Par
import Hackage4Nix.Nix.ErrM
import Hackage4Nix.Nix.Abs
import Cabal2Nix.Package
import Cabal2Nix.License
import Data.List
import Distribution.Package
import Distribution.Version

nixExpr2CabalExpr :: Expr -> Pkg
nixExpr2CabalExpr (Expr _ _ dict) =
    Pkg name version sha256 homepage description license isLib isExe
        deps buildTools extraLibs pkgconfDeps platforms maintainers
  where
    name = getString "pname" dict
    version = getString "version" dict
    sha256 = getString "sha256" dict
    homepage = getURL "homepage" (getDict "meta" dict)
    description = getString "description" (getDict "meta" dict)
    license = getString "license" (getDict "meta" dict)
    isLib = getBool "isLibrary" dict
    isExe = getBool "isExecutable" dict
    deps = [ Dependency (PackageName dep) anyVersion | dep <- getRefList "buildDepends" dict ]
    buildTools = [ Dependency (PackageName dep) anyVersion | dep <- getRefList "buildTools" dict ]
    extraLibs = getRefList "extraLibraries" dict
    pkgconfDeps = getRefList "pkgconfDepends" dict
    platforms = getRefList "platforms" (getDict "meta" dict)
    maintainers = getRefList "maintainers" (getDict "meta" dict)

get' ref (Dictionary attrs) = [ v | Attribute (Reference r) v <- attrs, r == [Ident ref] ]
get ref dict
  | [v] <-  get' ref dict = v
  | otherwise             = error $ "attribute " ++ ref ++ " doesn't exist in " ++ show dict
getBool ref dict = let BoolV b = get ref dict in if b == Yes then True else False
getString ref dict = let StringV x = get ref dict in x
getDict ref dict = let DictionaryV x = get ref dict in x
getURL ref dict
  | URLV (URL x) <- get ref dict = x
  | StringV x <- get ref dict = x

getRefList ref dict =
  let vals = get' ref dict
      rs = [ refs | ListV vs <- vals, AttributeV refs <- vs ]
  in
    map flattenReference rs

flattenReference (Reference rs) = concat (intersperse "." [ r | Ident r <- rs ])

parseNixExpr :: Monad m => String -> m Expr
parseNixExpr = run pExpr

type ParseFun a = [Token] -> Err a

run :: Monad m => ParseFun a -> String -> m a
run p s = case p (myLexer s) of
            Bad err    -> fail err
            Ok  tree   -> return tree

runTest = do
  buf <- readFile "/home/simons/.nix-defexpr/pkgs/development/libraries/haskell/hsemail/default.nix"
  tree <- parseNixExpr buf
  print tree
  let pkg = nixExpr2CabalExpr tree
  putStr (showNixPkg pkg)
