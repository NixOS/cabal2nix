module Main ( main ) where

import Cartel
import Control.Monad.Trans ( liftIO )
import System.Process ( readProcess )
import Data.List ( intercalate )

getVersion :: IO Version
getVersion = do
  v <- readProcess "git" ["log", "-1", "--format=%ci"] ""
  let y4:y3:y2:y1:'-':m2:m1:'-':d2:d1:[] = head (words v)
  return (read ('[':y4:y3:y2:y1:',':m2:m1:',':d2:d1:"]"))

getAuthors :: IO [NonEmptyString]
getAuthors = do
  buf <- readProcess "sh" ["-c", "git log --pretty=short | git shortlog --numbered --summary"] ""
  return [ unwords (tail (words l)) | l <- lines buf, not (null l) ]

properties :: Betsy IO Properties
properties = do
  v <- liftIO getVersion
  as <- liftIO $ getAuthors
  return $ blank
    { name = "cabal2nix"
    , version = v
    , buildType = Just simple
    , category = "Distribution"
    , maintainer = "Peter Simons <simons@cryp.to>"
    , synopsis = "Convert Cabal files into Nix build instructions."
    , description = [ "Convert Cabal files into Nix build instructions." ]
    , license = Just bsd3
    , licenseFile = "LICENSE"
    , copyright = "Copyright (c) 2011-2015 by Peter Simons et al"
    , author = intercalate ", " as
    , stability = "Experimental"
    , cabalVersion = Just (1,8)
    , homepage = "http://github.com/NixOS/cabal2nix/"
    , bugReports = "http://github.com/NixOS/cabal2nix/issues"
    , extraSourceFiles = ["README.md"]
    }

commonBuildOptions :: HasBuildInfo a => [a]
commonBuildOptions = hsSourceDirs ["src"] : commonOptions

commonOptions :: HasBuildInfo a => [a]
commonOptions = commonGhcOptions : commonBuildDepends : []

commonGhcOptions :: HasBuildInfo a => a
commonGhcOptions = ghcOptions ["-Wall"]

commonBuildDepends :: HasBuildInfo a => a
commonBuildDepends = buildDepends
  [ package "base" (lt [5])
  , unconstrained "aeson"
  , unconstrained "bytestring"
  , unconstrained "Cabal"
  , unconstrained "containers"
  , unconstrained "deepseq-generics"
  , unconstrained "directory"
  , unconstrained "filepath"
  , unconstrained "hackage-db"
  , unconstrained "monad-par"
  , unconstrained "monad-par-extras"
  , unconstrained "mtl"
  , unconstrained "pretty"
  , unconstrained "prettyclass"
  , unconstrained "process"
  , unconstrained "regex-posix"
  , unconstrained "SHA"
  , unconstrained "split"
  , unconstrained "transformers"
  , unconstrained "utf8-string"
  ]

commonTestOptions :: HasBuildInfo a => [a]
commonTestOptions = buildDepends
  [ unconstrained "deepseq"
  , unconstrained "doctest"
  , unconstrained "hspec"
  , unconstrained "QuickCheck"
  ] : commonBuildOptions

library :: Betsy IO [LibraryField]
library = do
  ms <- modules "src"
  return $ exposedModules ms : commonBuildOptions

mkExecutable :: NonEmptyString -> [ExecutableField] -> Section
mkExecutable exe opt = executable exe $ mainIs (exe++".hs") : opt ++ commonBuildOptions

mkTest :: NonEmptyString -> [TestSuiteField] -> Section
mkTest test opt = testSuite test $ exitcodeFields (test++".hs") ++ opt ++ hsSourceDirs ["test"] : commonTestOptions

main :: IO ()
main = defaultMain $ do
  props <- properties
  lib <- library
  return ( props, lib
         , [ githubHead "NixOS" "cabal2nix"
           , mkExecutable "cabal2nix" []
           , mkExecutable "hackage2nix" [ ghcOptions ["-threaded -rtsopts -with-rtsopts=-N"] ]
           , mkTest "spec" []
           , mkTest "doc-test" []
           ]
         )
