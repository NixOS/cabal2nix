module Main ( main ) where

import Control.Exception ( assert )
import Control.Monad ( when, filterM )
import Data.Char ( toLower )
import Data.List ( nubBy, sortBy )
import Data.Ord ( comparing )
import Data.Version ( Version(..) )
import Distribution.Package ( PackageIdentifier(..), PackageName(..) )
import Distribution.Text ( simpleParse, display )
import System.Directory ( doesDirectoryExist, doesFileExist )
import System.FilePath ( (</>), (<.>) )
import System.Process ( readProcess )
import Text.Regex.Posix ( (=~), match, makeRegexOpts, compExtended, execBlank )

type Pkg    = (String,Version,String) -- (Name, Version, Attribute)
type Pkgset = [Pkg]

comparePkgByVersion :: Pkg -> Pkg -> Ordering  -- prefers the latest version
comparePkgByVersion (n1,v1,a1) (n2,v2,a2)
  | a1 == a2    = assert (n1 == n2) (compare v2 v1)
  | otherwise   = compare a2 a1

comparePkgByName :: Pkg -> Pkg-> Ordering
comparePkgByName (n1,_,_) (n2,_,_) = comparing (map toLower) n1 n2

parseHaskellPackageName :: String -> Maybe Pkg
parseHaskellPackageName name =
  case name `regsubmatch` "^(haskellPackages[^ \t]+)[ \t]+(.+)$" of
    [attr,name'] -> case name' `regsubmatch` "^haskell-(.+)-ghc[0-9.]+-(.+)$" of
                      [name'',version] -> case simpleParse version of
                                            Just version' -> Just (name'',version',attr)
                                            _             -> error ("cannot parse " ++ show name)
                      _                -> case simpleParse name' of
                                            Just (PackageIdentifier (PackageName n) v) -> Just (n,v,attr)
                                            _                                          -> error ("cannot parse " ++ show name)
    _            -> Nothing

getHaskellPackageList :: IO Pkgset
getHaskellPackageList = do
  allPkgs <- fmap lines (readProcess "bash" ["-c", "exec nix-env -qaP \\* 2>/dev/tty"] "")
  return [ p | Just p <- map parseHaskellPackageName allPkgs ]

stripProfilingVersions :: Pkgset -> Pkgset
stripProfilingVersions pkgs = [ p | p@(_,_,attr) <- pkgs , not (attr =~ "ghc[0-9.]+_profiling") ]

stripGhc721Versions :: Pkgset -> Pkgset
stripGhc721Versions pkgs = [ p | p@(_,_,attr) <- pkgs , not (attr =~ "ghc721") ]

selectLatestVersions :: Pkgset -> Pkgset
selectLatestVersions = nubBy (\x y -> comparePkgByName x y == EQ) . sortBy comparePkgByVersion

isHackagePackage :: Pkg -> IO Bool
isHackagePackage (name,version,_) = doesFileExist path
  where path = "/dev/shm/hackage/" </> name </> display version </> name <.> "cabal"

formatPackageLine :: Pkg -> String
formatPackageLine (name,version,attr) = show (name, display version, Just url)
  where
    url = "http://hydra.nixos.org/job/nixpkgs/trunk/" ++ attr

regsubmatch :: String -> String -> [String]
regsubmatch buf patt = let (_,_,_,x) = f in x
  where f :: (String,String,String,[String])
        f = match (makeRegexOpts compExtended execBlank patt) buf

main :: IO ()
main = do
  haveHackage <- doesDirectoryExist "/dev/shm/hackage"
  when (not haveHackage) (fail "cannot find hackage database at /dev/shm/hackage")
  pkgset' <- fmap (selectLatestVersions . stripGhc721Versions . stripProfilingVersions) getHaskellPackageList
  pkgset <- filterM isHackagePackage pkgset'
  mapM_ (putStrLn . formatPackageLine) (sortBy comparePkgByName pkgset)
