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
import Distribution.Text
import Data.Version
import Data.List
import Data.Maybe
import Control.Monad
import Network.HTTP
import System.Process

import Cabal2Nix.Package

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
    libDeps = maybeToList (condLibrary cabal)
    exeDeps = [ tree | (_,tree) <- condExecutables cabal ]
    libs = concat [ extraLibs (libBuildInfo (condTreeData x)) | x <- libDeps ]
    libs' = concat [ extraLibs (buildInfo (condTreeData x)) | x <- exeDeps ]

simplify :: CondTree ConfVar [Dependency] a -> CondTree ConfVar [Dependency] ()
simplify (CondNode _ deps nodes) = CondNode () deps (map simp nodes)
  where
    simp (cond,tree,mtree) = (cond, simplify tree, maybe Nothing (Just . simplify) mtree)

data Ext = TarGz | Cabal deriving Eq

showExt :: Ext -> String
showExt TarGz = ".tar.gz"
showExt Cabal = ".cabal"

hackagePath :: PackageIdentifier -> Ext -> String
hackagePath (PackageIdentifier (PackageName name) version') ext =
    "http://hackage.haskell.org/packages/archive/" ++
    name ++ "/" ++ version ++ "/" ++ name ++
    (if ext == TarGz then "-" ++ version else "") ++
    showExt ext
  where
    version = showVersion version'

readCabalFile :: FilePath -> IO String
readCabalFile path
  | "cabal://" `isPrefixOf` path = let Just pid = simpleParse (drop 8 path) in readCabalFile (hackagePath pid Cabal)
  | "http://"  `isPrefixOf` path = Network.HTTP.simpleHTTP (getRequest path) >>= getResponseBody
  | "file://"  `isPrefixOf` path = readCabalFile (drop 7 path)
  | otherwise                    = readFile path

hashPackage :: GenericPackageDescription -> IO String
hashPackage pkg = do
    hash <- readProcess "bash" ["-c", "exec nix-prefetch-url 2>/dev/tty " ++ url] ""
    return (reverse (dropWhile (=='\n') (reverse hash)))

  where
    url = hackagePath pid TarGz
    pid = package (packageDescription pkg)

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


