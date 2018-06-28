{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.Haskell.Derivation
import Distribution.Nixpkgs.Haskell.FromCabal
import Distribution.Nixpkgs.Haskell.FromCabal.Flags

import Control.Lens
import Control.Monad
import qualified Data.Set as Set
import Distribution.Compiler
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.System
import Distribution.Verbosity
import Distribution.Version
import Language.Nix
import System.Directory
import System.FilePath
import System.Process
import Test.Tasty
import Test.Tasty.Golden
import Text.PrettyPrint.HughesPJClass

main :: IO ()
main = do
  -- TODO: run this test with all kinds of setLocaleEncoding values to ensure we don't
  -- depend on the system environment: https://github.com/NixOS/cabal2nix/issues/333
  cabal2nix <- findExecutable "cabal2nix" >>= maybe (fail "cannot find 'cabal2nix' in $PATH") return
  testCases <- listDirectoryFilesBySuffix ".cabal" "test/golden-test-cases"
  defaultMain $ testGroup "regression-tests"
    [ testGroup "cabal2nix library" (map testLibrary testCases)
    , testGroup "cabal2nix executable" (map (testExecutable cabal2nix) testCases)
    ]

testLibrary :: String -> TestTree
testLibrary cabalFile = do
  let nixFile = cabalFile `replaceExtension` "nix"
      goldenFile = nixFile `addExtension` "golden"

      cabal2nix :: GenericPackageDescription -> Derivation
      cabal2nix gpd = fromGenericPackageDescription
                         (const True)
                         (\i -> Just (binding # (i, path # [i])))
                         (Platform X86_64 Linux)
                         (unknownCompilerInfo (CompilerId GHC (mkVersion [8,2])) NoAbiTag)
                         (configureCabalFlags (packageId gpd))
                         []
                         gpd
                       & src .~ DerivationSource
                                  { derivKind     = "url"
                                  , derivUrl      = "mirror://hackage/foo.tar.gz"
                                  , derivRevision = ""
                                  , derivHash     = "deadbeef"
                                  }
                       & extraFunctionArgs %~ Set.union (Set.singleton "inherit stdenv")
  goldenVsFileDiff
    nixFile
    (\ref new -> ["diff", "-u", ref, new])
    goldenFile
    nixFile
    (readGenericPackageDescription silent cabalFile >>= writeFileLn nixFile . prettyShow . cabal2nix)

-- | TODO:
--
--  * Run cabal2nix in an empty environment. Currently, this fails because the
--    tool uses $HOME to write cache files for SHA256 hashes. To fix this, use
--    of the cache directory ought to be optional. Arguably, the whole notion
--    of caching those hashes is a bit silly in the presense of format 01
--    cabal-install tarballs which contain them already.
--
--  * If we cannot get along without $HOME, then we should at least set it to
--    some temporary location that is empty.
--
--  * Run this test with different kinds of encodings.


testExecutable :: FilePath -> FilePath -> TestTree
testExecutable exe cabalFile = do
  let nixFile = cabalFile `replaceExtension` "nix"
      goldenFile = nixFile `addExtension` "golden"
  goldenVsFileDiff
    nixFile
    (\ref new -> ["diff", "-u", ref, new])
    goldenFile
    nixFile
    (callCommand (unwords ["env LANG=en_US.UTF-8", exe, "--sha256=deadbeef", "--compiler=ghc-8.2", "--", cabalFile, ">"++nixFile]))

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


-- | A variant of 'writeFile' that appends a newline to the end of the buffer
-- before writing.

writeFileLn :: FilePath -> String -> IO ()
writeFileLn p buf = writeFile p (buf ++ "\n")
