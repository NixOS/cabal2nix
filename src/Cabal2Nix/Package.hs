module Cabal2Nix.Package ( cabal2nix, showNixPkg ) where

import Data.List
import Data.Maybe
import Distribution.Compiler
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.System
import Distribution.Version
import Text.PrettyPrint

import Cabal2Nix.License
import Cabal2Nix.Name
import Cabal2Nix.CorePackages

type PkgName         = String
type PkgVersion      = [Int]
type PkgSHA256       = String
type PkgURL          = String
type PkgDescription  = String
type PkgLicense      = License
type PkgDependencies = [Dependency] -- [CondTree ConfVar [Dependency] ()]
type PkgExtraLibs    = [String]
type PkgPlatforms    = [String]
type PkgMaintainers  = [String]

data Pkg = Pkg PkgName
               PkgVersion
               PkgSHA256
               PkgURL
               PkgDescription
               PkgLicense
               PkgDependencies
               PkgExtraLibs
               PkgPlatforms
               PkgMaintainers
  deriving (Show)

showNixPkg :: Pkg -> String
showNixPkg (Pkg name ver sha256 url desc lic deps libs platforms maintainers) = render doc
  where
    doc = braces (fsep $ punctuate comma $ map text ("cabal" : pkgDeps)) <+>
            colon $$ text "" $$
          vcat [
            text "cabal.mkDerivation" <+> lparen <> text "self" <+>
              colon <+> lbrace,
            nest 2 $ vcat [
              attr "pname"   $ doubleQuotes (text name),
              attr "version" $ doubleQuotes showVer,
              attr "sha256"  $ doubleQuotes (text sha256),
              onlyIf pkgDeps $
                sep [
                  text "propagatedBuildInputs" <+> equals <+> lbrack,
                  nest 2 $ fsep $ map text pkgDeps,
                  rbrack <> semi
                ],
              vcat [
                text "meta" <+> equals <+> lbrace,
                nest 2 $ vcat [
                  onlyIf url  $ attr "homepage"    $ doubleQuotes (text url),
                  onlyIf desc $ attr "description" $ doubleQuotes (text desc),
                  attr "license" $ text (showLic lic),
                  onlyIf platforms $
                    sep [
                      text "platforms" <+> equals,
                      nest 2 ((fsep $ punctuate (text " ++") $ map text platforms)) <> semi
                    ],
                  onlyIf maintainers $
                    sep [
                      text "maintainer" <+> equals <+> lbrack,
                      nest 2 $ fsep $ map text maintainers,
                      rbrack <> semi
                    ]
                  ],
                rbrace <> semi
              ]
            ],
            rbrace <> rparen,
            text ""
          ]
    attr n v = text n <+> equals <+> v <> semi
    onlyIf p d = if not (null p) then d else empty
    showVer = hcat (punctuate (text ".") (map int ver))
    pkgDeps :: [String]
    pkgDeps = (nub $ sort $ map libNixName libs) ++
              (nub $ sort $ map toNixName $
               filter (`notElem` corePackages) $ map unDep deps)


cabal2nix :: GenericPackageDescription -> PkgSHA256 -> PkgPlatforms -> PkgMaintainers -> Pkg
cabal2nix cabal sha256 platforms maintainers =
    Pkg pkgname pkgver sha256 url desc lic
      (buildDepends tpkg)
      libs
      [ "self.stdenv.lib.platforms." ++ p | p <- platforms ]
      [ "self.stdenv.lib.maintainers." ++ m | m <- maintainers ]
  where
    pkg = packageDescription cabal
    PackageName pkgname = pkgName (package pkg)
    pkgver = versionBranch (pkgVersion (package pkg))
    lic = license pkg
    url = homepage pkg
    desc = synopsis pkg
    -- globalDeps = buildDepends pkg
    -- Potentially dangerous: determine a default flattening of the
    -- package description. Better approach: export the conditional
    -- structure and reflect it in the generated file.
    Right (tpkg, _) = finalizePackageDescription [] (const True)
                        (Platform I386 Linux) -- shouldn't be hardcoded
                        (CompilerId GHC (Version [7,0,4] [])) -- dito
                        [] cabal
    libDeps = map libBuildInfo $ maybeToList (library tpkg)
    exeDeps = map    buildInfo $ executables tpkg
    libsExtra =             concatMap extraLibs        (libDeps ++ exeDeps)
    libsPkgC  = map unDep $ concatMap pkgconfigDepends (libDeps ++ exeDeps)
    -- add pkgconfig as an extra dependency if there are any pkgconfig deps
    libs = libsExtra ++ (if null libsPkgC then []
                                          else ("pkgconfig" : libsPkgC))

unDep :: Dependency -> String
unDep (Dependency (PackageName x) _) = x

simplify :: CondTree ConfVar [Dependency] a -> CondTree ConfVar [Dependency] ()
simplify (CondNode _ deps nodes) = CondNode () deps (map simp nodes)
  where
    simp (cond,tree,mtree) = (cond, simplify tree, maybe Nothing (Just . simplify) mtree)
