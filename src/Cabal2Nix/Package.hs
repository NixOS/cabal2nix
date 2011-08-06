module Cabal2Nix.Package where

import Data.List
import Data.Maybe

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version

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
