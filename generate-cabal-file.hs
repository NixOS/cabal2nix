module Main ( main ) where

import Cartel
import Control.Monad.Trans ( liftIO )
import System.Process ( readProcess )
import Data.List ( intercalate )
import System.Environment ( getArgs )

getVersion :: IO Version
getVersion = do
  v <- readProcess "git" ["log", "-1", "--format=%ci"] ""
  let [y4,y3,y2,y1,'-',m2,m1,'-',d2,d1] = head (words v)
  return (read ('[':y4:y3:y2:y1:m2:m1:d2:d1:"]"))

getGitVersion :: IO String
getGitVersion = do
  [v] <- fmap lines (readProcess "git" ["describe", "--dirty"] "")
  return v

getAuthors :: IO [NonEmptyString]
getAuthors = do
  buf <- readProcess "sh" ["-c", "git log --pretty=short | git shortlog --numbered --summary"] ""
  return [ unwords (tail (words l)) | l <- lines buf, not (null l) ]

properties :: Betsy IO Properties
properties = do
  v <- liftIO getVersion
  as <- liftIO getAuthors
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
    , cabalVersion = Just (1,10)
    , homepage = "http://github.com/NixOS/cabal2nix/"
    , bugReports = "http://github.com/NixOS/cabal2nix/issues"
    , extraSourceFiles = ["README.md"]
    , testedWith = [(ghc, eq [7,10,2])]
    }

commonBuildOptions :: HasBuildInfo a => [a]
commonBuildOptions = hsSourceDirs ["src"] : haskell2010 : commonOptions

commonOptions :: HasBuildInfo a => [a]
commonOptions = commonGhcOptions : commonBuildDepends

commonGhcOptions :: HasBuildInfo a => a
commonGhcOptions = ghcOptions ["-Wall"]

commonBuildDepends :: HasBuildInfo a => [a]
commonBuildDepends =
  [ buildDepends
    [ package "base" (lt [5])
    , unconstrained "aeson"
    , unconstrained "ansi-wl-pprint"
    , unconstrained "bytestring"
    , package "Cabal" (gtEq [1,22,2])
    , unconstrained "containers"
    , unconstrained "deepseq-generics"
    , unconstrained "directory"
    , unconstrained "filepath"
    , unconstrained "hackage-db"
    , unconstrained "lens"
    , unconstrained "monad-par"
    , unconstrained "abstract-par"
    , unconstrained "mtl"
    , unconstrained "optparse-applicative"
    , package "pretty" (gtEq [1,1,2])
    , unconstrained "process"
    , unconstrained "regex-posix"
    , unconstrained "SHA"
    , unconstrained "split"
    , unconstrained "transformers"
    , unconstrained "utf8-string"
    ]
  ]

commonTestOptions :: HasBuildInfo a => [a]
commonTestOptions = buildDepends
  [ unconstrained "deepseq"
  , unconstrained "doctest"
  , unconstrained "hspec"
  , unconstrained "QuickCheck"
  ] : commonBuildOptions

mkExecutable :: NonEmptyString -> [ExecutableField] -> Section
mkExecutable exe opt = executable exe $ mainIs (exe++".hs") : opt ++ commonBuildOptions

mkTest :: NonEmptyString -> [TestSuiteField] -> Section
mkTest test opt = testSuite test $ exitcodeFields (test++".hs") ++ opt ++ hsSourceDirs ["test"] : commonTestOptions

main :: IO ()
main = do
  args <- getArgs
  let releaseMode = "--release" `elem` args
  defaultMainWithHeader (const (return "")) $ do
    liftIO $ do ('v':gv) <- getGitVersion
                writeFile "src/Cabal2Nix/Version.hs" $
                  "module Cabal2Nix.Version where\n\
                  \version :: String\n\
                  \version = " ++ show gv ++ "\n"
    libraryModules <- modules "src"
    liftIO $ writeFile "test/doctest.hs" (mkDoctest libraryModules)
    props <- properties
    return ( props
           , exposedModules libraryModules : commonBuildOptions
           , [ githubHead "NixOS" "cabal2nix"
             , mkExecutable "cabal2nix" []
             , mkExecutable "hackage2nix" [ buildable (not releaseMode)
                                          , ghcOptions ["-threaded", "-rtsopts", "-with-rtsopts=-N"]
                                          ]
             , mkTest "spec" []
             , mkTest "doctest" []
             ]
           )

mkDoctest :: [NonEmptyString] -> String
mkDoctest libraryModules =
  let paths = [ "src/" ++ map toPath l ++ ".hs" | l <- libraryModules ]
      toPath '.' = '/'
      toPath c   = c
  in
    "module Main ( main ) where\n\
    \import Test.DocTest\n\
    \main :: IO ()\n\
    \main = do\n\
    \  doctest $ [\"-isrc\", \"-optP-include\", \"-optPdist/build/autogen/cabal_macros.h\"] ++ " ++ show paths ++ "\n\
    \  doctest [\"-isrc\", \"-optP-include\", \"-optPdist/build/autogen/cabal_macros.h\", \"src/cabal2nix.hs\"]\n\
    \  doctest [\"-isrc\", \"-optP-include\", \"-optPdist/build/autogen/cabal_macros.h\", \"src/hackage2nix.hs\"]\n"
