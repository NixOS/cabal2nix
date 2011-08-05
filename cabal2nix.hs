module Main ( main ) where

import System.Environment
import Distribution.PackageDescription.Parse
import Distribution.Verbosity ( silent )
import Distribution.PackageDescription
import Distribution.Package
import Distribution.License
import Data.Version
import Data.List

data Pkg = Pkg String [Int] String String String License [CondTree ConfVar [Dependency] ()] [String]

instance Show Pkg where
  show (Pkg name ver sha256 url desc lic deps libs) =
       "  " ++ attrib ++ " = callPackage ({cabal," ++ depArgs ++"}: cabal.mkDerivation (self : {\n"
    ++ "    pname = " ++ show name ++ ";\n"
    ++ "    version = \"" ++ showVer ++ "\";\n"
    ++ "    sha256 = " ++ show sha256 ++ ";\n"
    ++ "    propagatedBuildInputs = [" ++ depList ++ "];\n"
    ++ "    meta = {\n"
    ++ "      homepage = \"" ++ url ++ "\";\n"
    ++ "      description = " ++ show desc ++ ";\n"
    ++ "      license = " ++ showLic lic ++ ";\n"
    ++ "      platforms = self.stdenv.lib.platforms.haskellPlatforms;\n"
    ++ "      maintainers = [ self.stdenv.lib.maintainers.simons ];\n"
    ++ "    };\n"
    ++ "  })) {};\n"
    where
      attrib  = concat (intersperse "_" (nixName name : (map show ver)))
      showVer = concat (intersperse "." (map show ver))
      depList = concat (intersperse " " pkgDeps)
      depArgs = concat (intersperse "," pkgDeps)
      pkgDeps :: [String]
      pkgDeps = map nixName $
                  nub $ sort $
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

      nixName = map (\c -> if c == '-' then '_' else c)

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

simplify :: CondTree ConfVar [Dependency] a -> CondTree ConfVar [Dependency] ()
simplify (CondNode _ deps nodes) = CondNode () deps (map simp nodes)
  where
    simp (cond,tree,mtree) = (cond, simplify tree, maybe Nothing (Just . simplify) mtree)

main :: IO ()
main = do
  cabalFile:sha256:[] <- getArgs
  pkg <- readPackage cabalFile sha256
  putStr (show pkg)
