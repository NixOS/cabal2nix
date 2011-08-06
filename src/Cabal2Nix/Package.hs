module Cabal2Nix.Package where

import Data.List
import Data.Maybe
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version
import Text.PrettyPrint

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
toNix (Pkg name ver sha256 url desc lic deps libs) = render doc
    where
      doc = braces (fsep $ punctuate comma $ map text ("cabal" : pkgDeps)) <+>
              colon $$ text "" $$
            vcat [
              text "cabal.mkDerivation" <+> lparen <> text "self" <+>
                colon <+> lbrace,
              nest 2 $ vcat [
                attr "pname"   $ doubleQuotes (text name),
                attr "version" $ showVer,
                attr "sha256"  $ doubleQuotes (text sha256),
                sep [
                  text "propagatedBuildInputs" <+> equals <+> lbrack,
                  nest 2 $ fsep $ map text pkgDeps,
                  rbrack <> semi
                ],
                vcat [
                  text "meta" <+> equals <+> lbrace,
                  nest 2 $ vcat [
                    attr "homepage"    $ doubleQuotes (text url),
                    attr "description" $ doubleQuotes (text desc),
                    attr "license"     $ text (showLic lic)
                  ],
                  rbrace <> semi
                ]
              ],
              rbrace <> rparen
            ]
      attr n v = text n <+> equals <+> v <> semi
      showVer = hcat (punctuate (text ".") (map int ver))
      pkgDeps :: [String]
      pkgDeps = nub $ sort $ map toNixName $
                libs ++ [ n | dep <- deps,
                              Dependency (PackageName n) _ <- condTreeConstraints dep,
                              n `notElem` corePackages
                        ]

-- | List of packages shipped with ghc and therefore at the moment not in
-- nixpkgs. This should probably be configurable at first. Later, it might
-- be good to actually include them as dependencies, but set them to null
-- if GHC provides them (as different GHC versions vary).
corePackages :: [String]
corePackages = [
    "array",
    "base",
    "bytestring",
    "Cabal",
    "containers",
    "directory",
    "extensible-exceptions",
    "filepath",
    "ghc-prim",
    "haskell2010",
    "haskell98",
    "hpc",
    "old-locale",
    "old-time",
    "pretty",
    "process",
    "random",
    "template-haskell",
    "time",
    "unix"
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
