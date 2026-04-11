{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Distribution.Nixpkgs.Haskell.Derivation
  ( FinalizedDerivation(..), finalized_compiler, finalized_derivation, finalized_flags, finalized_platform
  , Derivation, nullDerivation, pkgid, revision, src, subpath, isLibrary, isExecutable
  , Component, Components, extraFunctionArgs, libraryDepends, executableDepends, testDepends, configureFlags
  , cabalFlags, runHaddock, jailbreak, doCheck, doBenchmark, testFlags, testTargets, hyperlinkSource
  , enableLibraryProfiling, enableExecutableProfiling, phaseOverrides, editedCabalFile, metaSection
  , dependencies, setupDepends, benchmarkDepends, enableSeparateDataOutput, extraAttributes
  , focusBuildInfo
  )
  where

import Prelude hiding ((<>))

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Control.DeepSeq
import Control.Lens
import Data.Foldable
import Data.List ( isPrefixOf )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Set.Lens
import Distribution.Compiler (CompilerInfo)
import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.Haskell.BuildInfo
import Distribution.Nixpkgs.Haskell.OrphanInstances ( )
import Distribution.Nixpkgs.Meta
import Distribution.Package
import Distribution.PackageDescription (CondBranch (..), CondTree (..), Condition (..), ConfVar (..), FlagAssignment, FlagName, lookupFlagAssignment, unFlagAssignment, unFlagName)
import Distribution.System (Platform (..))
import GHC.Generics ( Generic )
import Language.Nix
import Language.Nix.PrettyPrinting
import Data.Maybe (fromMaybe)
import Distribution.PackageDescription.Configuration (simplifyWithSysParams)

-- | A represtation of Nix expressions for building Haskell packages.
-- The data type correspond closely to the definition of
-- 'PackageDescription' from Cabal.
data FinalizedDerivation = FinalizedDerivation
  { _finalized_flags :: FlagAssignment
  , _finalized_platform :: Platform
  , _finalized_compiler :: CompilerInfo
  , _finalized_derivation :: Derivation
  }

type Components = [Component]
type Component = CondTree ConfVar [Dependency] (BuildInfo, Bool)
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
  , _libraryDepends             :: Components
  , _executableDepends          :: Components
  , _testDepends                :: Components
  , _benchmarkDepends           :: Components
  , _configureFlags             :: Set String
  , _cabalFlags                 :: FlagAssignment
  , _runHaddock                 :: Bool
  , _jailbreak                  :: Bool
  , _doCheck                    :: Bool
  , _doBenchmark                :: Bool
  , _testFlags                  :: [String]
  , _testTargets                :: [String]
  , _hyperlinkSource            :: Bool
  , _enableLibraryProfiling     :: Bool
  , _enableExecutableProfiling  :: Bool
  , _phaseOverrides             :: String
  , _editedCabalFile            :: String
  , _enableSeparateDataOutput   :: Bool
  , _metaSection                :: Meta
  }
  deriving (Show, Generic)

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
  , _testFlags = error "undefined Derivation.testFlags"
  , _testTargets = error "undefined Derivation.testTargets"
  , _hyperlinkSource = error "undefined Derivation.hyperlinkSource"
  , _enableLibraryProfiling = error "undefined Derivation.enableLibraryProfiling"
  , _enableExecutableProfiling = error "undefined Derivation.enableExecutableProfiling"
  , _phaseOverrides = error "undefined Derivation.phaseOverrides"
  , _editedCabalFile = error "undefined Derivation.editedCabalFile"
  , _enableSeparateDataOutput = error "undefined Derivation.enableSeparateDataOutput"
  , _metaSection = error "undefined Derivation.metaSection"
  }

makeLenses ''FinalizedDerivation

makeLenses ''Derivation

makeLensesFor (fmap (,"nonSetupDependencies") ["_libraryDepends", "_executableDepends", "_testDepends", "_benchmarkDepends"]) ''Derivation

dependencies :: Traversal' Derivation BuildInfo
dependencies = traversal $ \focus drv ->
  liftA2 (set setupDepends) (focus $ view setupDepends drv) ((nonSetupDependencies . traverse . traverse . _1) focus drv)

focusBuildInfo :: Lens' Derivation Components -> Traversal' Derivation BuildInfo
focusBuildInfo l = l . traverse . traverse . _1

instance Package Derivation where
  packageId = view pkgid

instance NFData Derivation

instance Pretty FinalizedDerivation where
  pPrint (FinalizedDerivation flags (Platform arch os) compiler  (MkDerivation {..})) = funargs (map text ("mkDerivation" : toAscList inputs)) $$ vcat
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
      , pPrintBuildInfo "setup" _setupDepends
      , pPrintBuildInfo "library" lib
      , pPrintBuildInfo "executable" exe
      , pPrintBuildInfo "test" test
      , pPrintBuildInfo "benchmark" bench
      , boolattr "enableLibraryProfiling" _enableLibraryProfiling _enableLibraryProfiling
      , boolattr "enableExecutableProfiling" _enableExecutableProfiling _enableExecutableProfiling
      , boolattr "doHaddock" (not _runHaddock) _runHaddock
      , boolattr "jailbreak" _jailbreak _jailbreak
      , boolattr "doCheck" (not _doCheck) _doCheck
      , boolattr "doBenchmark" _doBenchmark _doBenchmark
      , onlyIf (not (null _testFlags)) $ listattr "testFlags" empty (map show _testFlags)
      , onlyIf (not (null _testTargets)) $ listattr "testTargets" empty (map show _testTargets)
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
                          , setOf (each . folded . localName . ident) $ fold [_setupDepends, lib, exe, test, bench]
                          , case derivKind _src of
                              Nothing -> mempty
                              Just derivKind' -> Set.fromList [derivKindFunction derivKind' | not isHackagePackage]
                          ]

      (lib, exe, test, bench) = over each (foldMap eval)
        (_libraryDepends, _executableDepends, _testDepends, _benchmarkDepends)

      renderedFlags = [ text "-f" <> (if enable then empty else char '-') <> text (unFlagName f) | (f, enable) <- unFlagAssignment _cabalFlags ]
                      ++ map text (toAscList _configureFlags)
      isHackagePackage = "mirror://hackage/" `isPrefixOf` derivUrl _src

      postUnpack = string $ "sourceRoot+=/" ++ _subpath ++ "; echo source root reset to $sourceRoot"

      eval :: Component -> BuildInfo
      eval = fold . evalTree

      evalTree :: Component -> Maybe BuildInfo
      evalTree (CondNode (bi, buildable) _ branches) = case buildable of
        False -> Nothing
        True -> do
          bs <- traverse evalBranch branches
          pure $ fold $ bi : bs

      evalBranch :: CondBranch ConfVar [Dependency] (BuildInfo, Bool) -> Maybe BuildInfo
      evalBranch (CondBranch c t mf) =
        if evalCondition c
          then evalTree t
          else maybe (Just mempty) evalTree mf

      evalCondition :: Condition ConfVar -> Bool
      evalCondition = go . fst . simplifyWithSysParams os arch compiler
        where
          go :: Condition FlagName -> Bool
          go = \case
            Lit b -> b
            CNot c -> not $ go c
            COr  a b -> go a || go b
            CAnd a b -> go a && go b
            Var fn -> fromMaybe False $ lookupFlagAssignment fn flags
