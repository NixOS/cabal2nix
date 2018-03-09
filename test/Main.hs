module Main ( main ) where

import Distribution.Nixpkgs.Haskell.FromCabal
import Distribution.Nixpkgs.Haskell.Derivation
import Distribution.Nixpkgs.Fetch

import Control.Lens
import Distribution.Compiler
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.System
import Distribution.Verbosity
import Distribution.Version
import Language.Nix
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Text.PrettyPrint.HughesPJClass

main :: IO ()
main = do
  -- TODO: run this test with all kinds of setLocaleEncoding values to ensure we don't
  -- depend on the system environment: https://github.com/NixOS/cabal2nix/issues/333
  testCases <- findByExtension [".cabal"] "test/golden-test-cases"
  defaultMain $ testGroup "regression-tests" (map regressionTest testCases)

regressionTest :: String -> TestTree
regressionTest cabalFile = do
  let nixFile = cabalFile `replaceExtension` "nix"
      goldenFile = nixFile `addExtension` "golden"

      cabal2nix :: GenericPackageDescription -> Derivation
      cabal2nix gpd = fromGenericPackageDescription
                        (const True)
                        (\i -> Just (binding # (i, path # [i])))
                        (Platform X86_64 Linux)
                        (unknownCompilerInfo (CompilerId GHC (mkVersion [8,2])) NoAbiTag)
                        mempty
                        []
                        gpd
                      & src .~ DerivationSource
                                 { derivKind     = "url"
                                 , derivUrl      = "http://example.org/"
                                 , derivRevision = ""
                                 , derivHash     = "abc"
                                 }
  goldenVsFileDiff
    nixFile
    (\ref new -> ["diff", "-u", ref, new])
    goldenFile
    nixFile
    (readGenericPackageDescription silent cabalFile >>= writeFile nixFile . prettyShow . cabal2nix)
