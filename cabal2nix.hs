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
import Distribution.PackageDescription
import Distribution.Package
import Distribution.License
import Data.Version
import Data.List
import Control.Monad
import Data.Char
import Network.HTTP
import System.Process

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
toNixName "Cabal" = "cabal"
toNixName name = f name
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
      exprArgs = concat (intersperse "," ("cabal":pkgDeps))
      showVer = concat (intersperse "." (map show ver))
      depList = concat (intersperse " " pkgDeps)
      pkgDeps :: [String]
      pkgDeps = filter (/="cabal") $ nub $ sort $ map toNixName $
                  libs ++ [ n | dep <- deps, Dependency (PackageName n) _ <- condTreeConstraints dep
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

cabal2nix :: GenericPackageDescription -> PkgSHA256 -> Pkg
cabal2nix cabal sha256 = Pkg pkgname pkgver sha256 url desc lic (map simplify libDeps ++ map simplify exeDeps) (libs++libs')
  where
    pkg = packageDescription cabal
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

simplify :: CondTree ConfVar [Dependency] a -> CondTree ConfVar [Dependency] ()
simplify (CondNode _ deps nodes) = CondNode () deps (map simp nodes)
  where
    simp (cond,tree,mtree) = (cond, simplify tree, maybe Nothing (Just . simplify) mtree)

readCabalFile :: FilePath -> IO String
readCabalFile path
  | "http://" `isPrefixOf` path = Network.HTTP.simpleHTTP (getRequest path) >>= getResponseBody
  | "file://" `isPrefixOf` path = readCabalFile (drop 7 path)
  | otherwise                   = readFile path

hashPackage :: GenericPackageDescription -> IO String
hashPackage pkg = readProcess "bash" ["-c", "exec nix-prefetch-url 2>/dev/tty " ++ url] ""
  where
    url = "http://hackage.haskell.org/packages/archive/" ++ name ++ "/" ++ version ++ "/" ++ name ++ "-" ++ version ++ ".tar.gz"
    PackageIdentifier (PackageName name) version' = package (packageDescription pkg)
    version = showVersion version'

main :: IO ()
main = bracket (return ()) (\() -> hFlush stdout >> hFlush stderr) $ \() -> do
  args <- getArgs

  let usage = "Usage: cabal2nix url-to-cabal-file [sha256-hash]"

  when (length args < 1 || length args > 2) $ do
    mapM_ (hPutStrLn stderr) [ "*** invalid command line syntax"
                             , usage
                             ]
    exitFailure

  when ("--help" `elem` args || "-h" `elem` args) $ do
    putStrLn usage
    exitFailure

  cabal' <- fmap parsePackageDescription (readCabalFile (head args))
  cabal <- case cabal' of
             ParseOk _ a -> return a
             ParseFailed err -> do
               hPutStrLn stderr ("*** cannot parse cabal file: " ++ show err)
               exitFailure

  sha256 <- case args of
              _:hash:[] -> return hash
              _         -> hashPackage cabal

  let pkg = cabal2nix cabal sha256
  putStr (toNix pkg)


