{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Distribution.Nixpkgs.Haskell.Derivation
  ( Derivation, nullDerivation, pkgid, revision, src, subpath, isLibrary, isExecutable
  , extraFunctionArgs, libraryDepends, executableDepends, testDepends, configureFlags
  , cabalFlags, runHaddock, jailbreak, doCheck, doBenchmark, testTarget, hyperlinkSource, enableSplitObjs
  , enableLibraryProfiling, enableExecutableProfiling, phaseOverrides, editedCabalFile, metaSection
  , dependencies, setupDepends, benchmarkDepends, enableSeparateDataOutput, extraAttributes
  )
  where

import Prelude hiding ((<>))

import Control.DeepSeq
import Control.Lens
import Data.List ( isPrefixOf )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Set.Lens
import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.Haskell.BuildInfo
import Distribution.Nixpkgs.Haskell.OrphanInstances ( )
import Distribution.Nixpkgs.Meta
import Distribution.Package
import Distribution.PackageDescription ( FlagAssignment, unFlagName, unFlagAssignment )
import GHC.Generics ( Generic )
import Language.Nix
import Language.Nix.PrettyPrinting

-- | A represtation of Nix expressions for building Haskell packages.
-- The data type correspond closely to the definition of
-- 'PackageDescription' from Cabal.

data Derivation = MkDerivation
  { _pkgid                      :: PackageIdentifier
  , _revision                   :: Int
  , _src                        :: DerivationSource
  , _subpath                    :: FilePath
  , _isLibrary                  :: Bool
  , _isExecutable               :: Bool
  , _extraFunctionArgs          :: Set Binding
  , _extraAttributes            :: Map String String
  , _setupDepends               :: BuildInfo
  , _libraryDepends             :: BuildInfo
  , _executableDepends          :: BuildInfo
  , _testDepends                :: BuildInfo
  , _benchmarkDepends           :: BuildInfo
  , _configureFlags             :: Set String
  , _cabalFlags                 :: FlagAssignment
  , _runHaddock                 :: Bool
  , _jailbreak                  :: Bool
  , _doCheck                    :: Bool
  , _doBenchmark                :: Bool
  , _testTarget                 :: String
  , _hyperlinkSource            :: Bool
  , _enableLibraryProfiling     :: Bool
  , _enableExecutableProfiling  :: Bool
  , _enableSplitObjs            :: Bool
  , _phaseOverrides             :: String
  , _editedCabalFile            :: String
  , _enableSeparateDataOutput   :: Bool
  , _metaSection                :: Meta
  }
  deriving (Show, Eq, Generic)

nullDerivation :: Derivation
nullDerivation = MkDerivation
  { _pkgid = error "undefined Derivation.pkgid"
  , _revision = error "undefined Derivation.revision"
  , _src = error "undefined Derivation.src"
  , _subpath = error "undefined Derivation.subpath"
  , _isLibrary = error "undefined Derivation.isLibrary"
  , _isExecutable = error "undefined Derivation.isExecutable"
  , _extraFunctionArgs = error "undefined Derivation.extraFunctionArgs"
  , _extraAttributes = error "undefined Derivation.extraAttributes"
  , _setupDepends = error "undefined Derivation.setupDepends"
  , _libraryDepends = error "undefined Derivation.libraryDepends"
  , _executableDepends = error "undefined Derivation.executableDepends"
  , _testDepends = error "undefined Derivation.testDepends"
  , _benchmarkDepends = error "undefined Derivation.benchmarkDepends"
  , _configureFlags = error "undefined Derivation.configureFlags"
  , _cabalFlags = error "undefined Derivation.cabalFlags"
  , _runHaddock = error "undefined Derivation.runHaddock"
  , _jailbreak = error "undefined Derivation.jailbreak"
  , _doCheck = error "undefined Derivation.doCheck"
  , _doBenchmark = error "undefined Derivation.doBenchmark"
  , _testTarget = error "undefined Derivation.testTarget"
  , _hyperlinkSource = error "undefined Derivation.hyperlinkSource"
  , _enableLibraryProfiling = error "undefined Derivation.enableLibraryProfiling"
  , _enableExecutableProfiling = error "undefined Derivation.enableExecutableProfiling"
  , _enableSplitObjs = error "undefined Derivation.enableSplitObjs"
  , _phaseOverrides = error "undefined Derivation.phaseOverrides"
  , _editedCabalFile = error "undefined Derivation.editedCabalFile"
  , _enableSeparateDataOutput = error "undefined Derivation.enableSeparateDataOutput"
  , _metaSection = error "undefined Derivation.metaSection"
  }

makeLenses ''Derivation

makeLensesFor [("_setupDepends", "dependencies"), ("_libraryDepends", "dependencies"), ("_executableDepends", "dependencies"), ("_testDepends", "dependencies"), ("_benchmarkDepends", "dependencies")] ''Derivation

instance Package Derivation where
  packageId = view pkgid

instance NFData Derivation

instance Pretty Derivation where
  pPrint drv@MkDerivation {..} = funargs (map text ("mkDerivation" : toCaseInsensitiveAscList inputs)) $$ vcat
    [ text "mkDerivation" <+> lbrace
    , nest 2 $ vcat
      [ attr "pname"   $ doubleQuotes $ pPrint (packageName _pkgid)
      , attr "version" $ doubleQuotes $ pPrint (packageVersion _pkgid)
      , pPrint _src
      , onlyIf (_subpath /= ".") $ attr "postUnpack" postUnpack
      , onlyIf (_revision > 0) $ attr "revision" $ doubleQuotes $ int _revision
      , onlyIf (not (null _editedCabalFile) && _revision > 0) $ attr "editedCabalFile" $ string _editedCabalFile
      , listattr "configureFlags" empty (map (show . show) renderedFlags)
      , boolattr "isLibrary" (not _isLibrary || _isExecutable) _isLibrary
      , boolattr "isExecutable" (not _isLibrary || _isExecutable) _isExecutable
      , boolattr "enableSeparateDataOutput" _enableSeparateDataOutput _enableSeparateDataOutput
      , onlyIf (_setupDepends /= mempty) $ pPrintBuildInfo "setup" _setupDepends
      , onlyIf (_libraryDepends /= mempty) $ pPrintBuildInfo "library" _libraryDepends
      , onlyIf (_executableDepends /= mempty) $ pPrintBuildInfo "executable" _executableDepends
      , onlyIf (_testDepends /= mempty) $ pPrintBuildInfo "test" _testDepends
      , onlyIf (_benchmarkDepends /= mempty) $ pPrintBuildInfo "benchmark" _benchmarkDepends
      , boolattr "enableLibraryProfiling" _enableLibraryProfiling _enableLibraryProfiling
      , boolattr "enableExecutableProfiling" _enableExecutableProfiling _enableExecutableProfiling
      , boolattr "enableSplitObjs"  (not _enableSplitObjs) _enableSplitObjs
      , boolattr "doHaddock" (not _runHaddock) _runHaddock
      , boolattr "jailbreak" _jailbreak _jailbreak
      , boolattr "doCheck" (not _doCheck) _doCheck
      , boolattr "doBenchmark" _doBenchmark _doBenchmark
      , onlyIf (not (null _testTarget)) $ attr "testTarget" $ string _testTarget
      , boolattr "hyperlinkSource" (not _hyperlinkSource) _hyperlinkSource
      , onlyIf (not (null _phaseOverrides)) $ vcat ((map text . lines) _phaseOverrides)
      , pPrint _metaSection
      , vcat [ attr k (text v) | (k,v) <- Map.toList _extraAttributes ]
      ]
    , rbrace
    ]
    where
      inputs :: Set String
      inputs = Set.unions [ Set.map (view (localName . ident)) _extraFunctionArgs
                          , setOf (dependencies . each . folded . localName . ident) drv
                          , Set.fromList ["fetch" ++ derivKind _src | derivKind _src /= "" && not isHackagePackage]
                          ]

      renderedFlags = [ text "-f" <> (if enable then empty else char '-') <> text (unFlagName f) | (f, enable) <- unFlagAssignment _cabalFlags ]
                      ++ map text (Set.toAscList _configureFlags)
      isHackagePackage = "mirror://hackage/" `isPrefixOf` derivUrl _src

      postUnpack = string $ "sourceRoot+=/" ++ _subpath ++ "; echo source root reset to $sourceRoot"
