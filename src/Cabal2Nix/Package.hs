module Cabal2Nix.Package
  ( cabal2nix, showNixPkg
  , PkgName, PkgVersion, PkgSHA256, PkgURL, PkgDescription, PkgLicense
  , PkgIsLib, PkgIsExe, PkgDependencies, PkgBuildTools, PkgExtraLibs
  , PkgPkgconfigDeps, PkgPlatforms, PkgMaintainers
  )
  where

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

type PkgName          = String
type PkgVersion       = [Int]
type PkgSHA256        = String
type PkgURL           = String
type PkgDescription   = String
type PkgLicense       = License
type PkgIsLib         = Bool
type PkgIsExe         = Bool
type PkgDependencies  = [Dependency] -- [CondTree ConfVar [Dependency] ()]
type PkgBuildTools    = [Dependency]
type PkgExtraLibs     = [String]
type PkgPkgconfigDeps = [String]
type PkgPlatforms     = [String]
type PkgMaintainers   = [String]

data Pkg = Pkg PkgName
               PkgVersion
               PkgSHA256
               PkgURL
               PkgDescription
               PkgLicense
               PkgIsLib
               PkgIsExe
               PkgDependencies
               PkgBuildTools
               PkgExtraLibs
               PkgPkgconfigDeps
               PkgPlatforms
               PkgMaintainers
  deriving (Show)

prepunctuate :: Doc -> [Doc] -> [Doc]
prepunctuate _ []     = []
prepunctuate p (d:ds) = d : map (p <>) ds


showNixPkg :: Pkg -> String
showNixPkg (Pkg name ver sha256 url desc lic isLib isExe deps
                tools libs pcs platforms maintainers) =
    render doc
  where
    doc = sep [
            lbrace <+> (fcat $ prepunctuate (comma <> text " ") $
                        map (nest 2 . text)
                            ("cabal" : pkgInputs)),
            rbrace <> colon
          ] $$
          vcat [
            text "",
            text "cabal.mkDerivation" <+> lparen <> text "self" <>
              colon <+> lbrace,
            nest 2 $ vcat [
              attr "pname"   $ doubleQuotes (text name),
              attr "version" $ doubleQuotes showVer,
              attr "sha256"  $ doubleQuotes (text sha256),
              boolattr "isLibrary"    (not isLib || isExe) isLib,
              boolattr "isExecutable" (not isLib || isExe) isExe,
              listattr "buildDepends"     pkgDeps,
              listattr "buildTools"       pkgBuildTools,
              listattr "extraLibraries"   pkgLibs,
              listattr "pkgconfigDepends" pkgPCs,
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
                  listattr "maintainers" maintainers
                ],
                rbrace <> semi
              ]
            ],
            rbrace <> rparen,
            text ""
          ]
    attr n v = text n <+> equals <+> v <> semi
    onlyIf p d = if not (null p) then d else empty
    boolattr n p v = if p then
                       attr n (if v then text "true" else text "false")
                     else empty
    listattr n vs = onlyIf vs $ sep [
                      text n <+> equals <+> lbrack,
                      nest 2 $ fsep $ map text vs,
                      rbrack <> semi
                    ]
    showVer = hcat (punctuate (text ".") (map int ver))
    pkgLibs       = nub $ sort $ concatMap libNixName libs
    pkgPCs        = nub $ sort $ concatMap libNixName pcs
    pkgDeps       = nub $ sort $ map toNixName $
                    filter (`notElem` (name : corePackages)) $ map unDep deps
    pkgBuildTools = nub $ sort $ map toNixName $
                    filter (`notElem` coreBuildTools) $ map unDep tools
    pkgInputs     = nub $ pkgLibs ++ pkgPCs ++ pkgBuildTools ++ pkgDeps


cabal2nix :: GenericPackageDescription -> PkgSHA256 -> PkgPlatforms -> PkgMaintainers -> Pkg
cabal2nix cabal sha256 platforms maintainers =
    Pkg pkgname pkgver sha256 url desc lic
      isLib isExe
      (buildDepends tpkg)
      tools
      libs
      pcs
      [ if '.' `elem` p then p else "self.stdenv.lib.platforms." ++ p | p <- platforms ]
      [ if '.' `elem` m then m else "self.stdenv.lib.maintainers." ++ m | m <- maintainers ]
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
    isLib   = isJust (library tpkg)
    isExe   = not (null (executables tpkg))
    libDeps = map libBuildInfo $ maybeToList (library tpkg)
    exeDeps = map    buildInfo $ executables tpkg
    libs    =             concatMap extraLibs        (libDeps ++ exeDeps)
    pcs     = map unDep $ concatMap pkgconfigDepends (libDeps ++ exeDeps)
    tools   =             concatMap buildTools       (libDeps ++ exeDeps)

unDep :: Dependency -> String
unDep (Dependency (PackageName x) _) = x
