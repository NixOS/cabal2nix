module Main ( main ) where

import Test.DocTest
import Test.Tasty
import Test.Tasty.HUnit

import Cabal2nix ( mainWithArgs )
import System.Exit ( ExitCode (..) )
import System.FilePath ( (</>) )
import System.Process ( readProcessWithExitCode )

main :: IO ()
main = doctest [ "-isrc", "src" ] >> defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [regressionTests]

regressionTests :: TestTree
regressionTests = testGroup "Regression"
  [ testCase "#274: Generating an expression from a local file is broken since --subpath was added" $ do
      parsec <- allCabalHashes "parsec/3.0.0/parsec.cabal"
      mainWithArgs [ "--sha256=dontworry", parsec]
  ]


allCabalHashes :: FilePath -> IO String
allCabalHashes cabalPath = do
  (exit, out, err) <- readProcessWithExitCode "nix-build" ["--no-out-link", "<nixpkgs>", "-A", "all-cabal-hashes"] ""
  case exit of
    ExitSuccess   -> return $ filter (/= '\n') $ out </> cabalPath
    ExitFailure _ -> error err
