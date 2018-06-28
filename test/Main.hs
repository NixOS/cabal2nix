module Main ( main ) where

import Distribution.Nixpkgs.Haskell.FromCabal
import Distribution.Nixpkgs.Haskell.Derivation
import Distribution.Nixpkgs.Fetch

import Control.Lens
import Control.Monad
import Distribution.Compiler
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.System
import Distribution.Verbosity
import Distribution.Version
import Language.Nix
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Text.PrettyPrint.HughesPJClass

main :: IO ()
main = do
  -- TODO: run this test with all kinds of setLocaleEncoding values to ensure we don't
  -- depend on the system environment: https://github.com/NixOS/cabal2nix/issues/333
  testCases <- listDirectoryFilesBySuffix ".cabal" "test/golden-test-cases"
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


-------------------------------------------------------------------------------
-- * Helper functions
-------------------------------------------------------------------------------

-- | Find all *files* in the given directory. This function does not recurse,
-- nor does it include the special entries @'.'@ or @'..'@ in the result.

listDirectoryFiles :: FilePath -> IO [FilePath]
listDirectoryFiles dirPath = do
  entries <- listDirectory dirPath
  filterM doesFileExist (map (dirPath </>) entries)

-- | Find all *files* in the given directory that end with the given suffix.
-- This function does not recurse, nor does it include the special entries
-- @'.'@ or @'..'@ in the result.
--
-- "Test.Tasty.Golden" offers 'findByExtension', which does something similar
-- (and arguably more general) as those two functions, but unfortunately that
-- function recurses through the directory hierarchy and in the process needs
-- plenty of stack space. There might be a space leak in that code, I am not
-- sure. Anyhow, since our test suite is supposed to run with 1K stack space,
-- we prefer our own simpler code.

listDirectoryFilesBySuffix :: String -> FilePath -> IO [FilePath]
listDirectoryFilesBySuffix suff =
  fmap (filter ((suff ==) . takeExtension)) . listDirectoryFiles
