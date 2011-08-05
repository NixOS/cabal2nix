-- cabal2nix.hs
--
-- Copyright (c) 20011 Peter Simons <simons@cryp.to>
-- See LICENSE file for licensing details.

module Main ( main ) where

import System.IO
import System.Environment
import Control.Exception
import System.Exit
import Distribution.PackageDescription.Parse
import Distribution.Verbosity ( silent )
import Distribution.PackageDescription
import Distribution.Package
import Distribution.License
import Data.Version
import Data.List
import Control.Monad
import Data.Char

type PkgName = String
type PkgVersion = [Int]
type PkgSHA256 = String
type PkgURL = String
type PkgDescription = String
type PkgLicense = License
type PkgDependencies = [CondTree ConfVar [Dependency] ()]
type PkgExtraLibs = [String]

data Pkg = Pkg PkgName PkgVersion PkgSHA256 PkgURL PkgDescription PkgLicense PkgDependencies PkgExtraLibs
  deriving (Show)

toNixName :: String -> String
toNixName [] = error "toNixName: empty string is not a valid argument"
toNixName name = f ((toLower (head name)) : tail name)
  where
    f []                            = []
    f ('-':c:cs) | c `notElem` "-"  = toUpper c : f cs
    f ('-':_)                       = error ("unexpected package name " ++ show name)
    f (c:cs)                        = c : f cs

toNix :: Pkg -> String
toNix (Pkg name ver sha256 url desc lic deps libs) =
       "{" ++ exprArgs ++"}:\n\n"
    ++ "cabal.mkDerivation (self : {\n"
    ++ "  pname = " ++ show name ++ ";\n"
    ++ "  version = \"" ++ showVer ++ "\";\n"
    ++ "  sha256 = " ++ show sha256 ++ ";\n"
    ++ "  propagatedBuildInputs = [" ++ depList ++ "];\n"
    ++ "  meta = {\n"
    ++ "    homepage = \"" ++ url ++ "\";\n"
    ++ "    description = " ++ show desc ++ ";\n"
    ++ "    license = " ++ showLic lic ++ ";\n"
    ++ "  };\n"
    ++ "})\n"
    where
      exprArgs = concat (intersperse "," ((if "cabal" `notElem` pkgDeps then ["cabal"] else []) ++ pkgDeps))
      showVer = concat (intersperse "." (map show ver))
      depList = concat (intersperse " " pkgDeps)
      pkgDeps :: [String]
      pkgDeps = map toNixName $
                  nub $ sort $ libs ++
                    [ n | dep <- deps, Dependency (PackageName n) _ <- condTreeConstraints dep
                        , n `notElem` ["base","containers"]
                    ]
      showLic (GPL Nothing)                     = show "GPL"
      showLic (GPL (Just (Version [2] [])))     = "self.stdenv.lib.licenses.gpl2"
      showLic (GPL (Just (Version [3] [])))     = "self.stdenv.lib.licenses.gpl3"
      showLic (LGPL Nothing)                    = show "LGPL"
      showLic (LGPL (Just (Version [2,1] [])))  = "self.stdenv.lib.licenses.lgpl21"
      showLic (LGPL (Just (Version [3] [])))    = "self.stdenv.lib.licenses.lgpl3"
      showLic BSD3                              = "self.stdenv.lib.licenses.bsd3"
      showLic BSD4                              = "self.stdenv.lib.licenses.bsd4"
      showLic MIT                               = "self.stdenv.lib.licenses.mit"
      showLic PublicDomain                      = "self.stdenv.lib.licenses.publicDomain"
      showLic AllRightsReserved                 = "unknown"
      showLic OtherLicense                      = "unknown"
      showLic l                                 = error $ "unknown license: " ++ show l

readPackage :: FilePath -> String -> IO Pkg
readPackage cabalFile sha256 = do
  cabal <- readPackageDescription silent cabalFile
  let pkg = packageDescription cabal
      PackageName pkgname = pkgName (package pkg)
      pkgver = versionBranch (pkgVersion (package pkg))
      lic = license pkg
      url = homepage pkg
      desc = synopsis pkg
      -- globalDeps = buildDepends pkg
      libDeps = maybe [] (\x -> [x]) (condLibrary cabal)
      exeDeps = [ tree | (_,tree) <- condExecutables cabal ]
      libs = concat [ extraLibs (libBuildInfo (condTreeData x)) | x <- libDeps ]
      libs' = concat [ extraLibs (buildInfo (condTreeData x)) | x <- exeDeps ]
  return (Pkg pkgname pkgver sha256 url desc lic (map simplify libDeps ++ map simplify exeDeps) (libs++libs'))
    where

simplify :: CondTree ConfVar [Dependency] a -> CondTree ConfVar [Dependency] ()
simplify (CondNode _ deps nodes) = CondNode () deps (map simp nodes)
  where
    simp (cond,tree,mtree) = (cond, simplify tree, maybe Nothing (Just . simplify) mtree)

main :: IO ()
main = bracket (return ()) (\() -> hFlush stdout >> hFlush stderr) $ \() -> do
  args <- getArgs

  when (length args /= 2) $ do
    mapM_ (hPutStrLn stderr) [ "*** invalid command line syntax"
                             , "Usage: cabal2nix cabal-file sha256-hash"
                             ]
    exitFailure

  let cabalFile:sha256:[] = args
  pkg <- readPackage cabalFile sha256
  putStr (toNix pkg)
