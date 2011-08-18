module Main ( main ) where

import Data.Char
import Data.List
import Data.Version
import Distribution.Text
import System.Process
import Text.Regex.Posix
import Control.Exception ( assert )

type Pkg    = (String,Version,String) -- (Name, Version, Attribute)
type Pkgset = [Pkg]

comparePkgByVersion :: Pkg -> Pkg -> Ordering  -- prefers the latest version
comparePkgByVersion (n1,v1,a1) (n2,v2,a2)
  | a1 == a2    = assert (n1 == n2) (compare v2 v1)
  | otherwise   = compare a2 a1

comparePkgByName :: Pkg -> Pkg-> Ordering
comparePkgByName (n1,_,_) (n2,_,_) = compare (map toLower n1) (map toLower n2)

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
  return [ p | Just p <- map parseHaskellPackageName allPkgs ]

stripProfilingVersions :: Pkgset -> Pkgset
stripProfilingVersions pkgs = [ p | p@(_,_,attr) <- pkgs , not (attr =~ "ghc[0-9.]+_profiling") ]

stripGhc721Versions :: Pkgset -> Pkgset
stripGhc721Versions pkgs = [ p | p@(_,_,attr) <- pkgs , not (attr =~ "ghc721") ]

selectLatestVersions :: Pkgset -> Pkgset
selectLatestVersions = nubBy (\x y -> comparePkgByName x y == EQ) . sortBy comparePkgByVersion

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
  pkgset <- fmap (selectLatestVersions . stripGhc721Versions . stripProfilingVersions) getHaskellPackageList
  mapM_ (putStrLn . formatPackageLine) (sortBy comparePkgByName pkgset)
