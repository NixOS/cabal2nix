module Cabal2Nix.Package where

import Data.List

import Distribution.Package
import Distribution.PackageDescription

import Cabal2Nix.License
import Cabal2Nix.Name

type PkgName         = String
type PkgVersion      = [Int]
type PkgSHA256       = String
type PkgURL          = String
type PkgDescription  = String
type PkgLicense      = License
type PkgDependencies = [CondTree ConfVar [Dependency] ()]
type PkgExtraLibs    = [String]

data Pkg = Pkg PkgName
               PkgVersion
               PkgSHA256
               PkgURL
               PkgDescription
               PkgLicense
               PkgDependencies
               PkgExtraLibs
  deriving (Show)

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

