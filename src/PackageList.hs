module Main ( main ) where

import Text.Regex.Posix
import System.Process
import Data.Maybe
import Data.List
import Data.Version
import Distribution.Text
import Distribution.Package
import Cabal2Nix.Name

regsubmatch :: String -> String -> [String]
regsubmatch buf patt = let (_,_,_,x) = f in x
  where f :: (String,String,String,[String])
        f = match (makeRegexOpts compExtended execBlank patt) buf

parseHaskellPackageName :: String -> Maybe (String,Version)
parseHaskellPackageName name =
  case name `regsubmatch` "^haskell-(.*)-ghc[0-9.]+-(.*)$" of
    [name',version] -> case simpleParse version of
                         Just version' -> Just (name',version')
                         _             -> Nothing
    _               -> Nothing

stripProfilingTag :: Version -> Version
stripProfilingTag (Version v tags) = Version v (filter (/="profiling") tags)

getHaskellPackageList :: IO [(String,Version)]
getHaskellPackageList = do
  allPkgs <- fmap lines (readProcess "bash" ["-c", "exec nix-env -qa \\* 2>/dev/tty"] "")
  let hsPkgs  = map parseHaskellPackageName allPkgs
      pkglist = [ (name, stripProfilingTag version) | Just (name,version) <- hsPkgs ]
  return (sort (nub pkglist))

selectLatestVersion :: [(String,Version)] -> [(String,Version)]
selectLatestVersion = nubBy f2 . sortBy f1
  where
    f1 (n1,v1) (n2,v2)
      | n1 == n2     = compare v2 v1
      | otherwise    = compare n1 n2
    f2 (n1,v1) (n2,v2) = n1 == n2

formatPackageLine :: (String,Version) -> String
formatPackageLine (n,v) = show (n, showVersion v, Just url)
  where
    url = "http://hydra.nixos.org/job/nixpkgs/trunk/haskellPackages_ghc704_no_profiling."++toNixName n

main :: IO ()
main = do
  pkgs <- fmap selectLatestVersion getHaskellPackageList
  mapM_ (putStrLn . formatPackageLine) pkgs
