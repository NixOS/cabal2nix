module Main ( main ) where

import Data.List
import Data.Maybe
import Data.Version
import Distribution.Text
import System.Process
import Text.Regex.Posix

type Pkg    = (String,Version,String) -- (Name, Version, Attribute)
type Pkgset = [Pkg]

parseHaskellPackageName :: String -> Maybe Pkg
parseHaskellPackageName name =
  case name `regsubmatch` "^(haskellPackages[^ \t]+)[ \t]+haskell-(.*)-ghc[0-9.]+-(.*)$" of
    [attr,name',version] -> case simpleParse version of
                              Just version' -> Just (name',version',attr)
                              _             -> Nothing
    _               -> Nothing

getHaskellPackageList :: IO Pkgset
getHaskellPackageList = do
  allPkgs <- fmap lines (readProcess "bash" ["-c", "exec nix-env -qaP \\* 2>/dev/tty"] "")
  let pkgset = map parseHaskellPackageName allPkgs
  return (sort (nub (catMaybes pkgset)))

stripProfilingVersions :: Pkgset -> Pkgset
stripProfilingVersions pkgs = [ p | p@(_,_,attr) <- pkgs , not ("ghc[0-9.]+_profiling" =~ attr) ]

selectLatestVersions :: Pkgset -> Pkgset
selectLatestVersions = nubBy f2 . sortBy f1
  where
    f1 (n1,v1,a1) (n2,v2,a2)
      | n1 == n2         = compare v2 v1
      | otherwise        = compare n1 n2
    f2 (n1,_,_) (n2,_,_) = n1 == n2

formatPackageLine :: Pkg -> String
formatPackageLine (name,version,attr) = show (name, showVersion version, Just url)
  where
    url = "http://hydra.nixos.org/job/nixpkgs/trunk/" ++ attr

regsubmatch :: String -> String -> [String]
regsubmatch buf patt = let (_,_,_,x) = f in x
  where f :: (String,String,String,[String])
        f = match (makeRegexOpts compExtended execBlank patt) buf

main :: IO ()
main = do
  pkgs <- fmap (selectLatestVersions . stripProfilingVersions) getHaskellPackageList
  mapM_ (putStrLn . formatPackageLine) pkgs
