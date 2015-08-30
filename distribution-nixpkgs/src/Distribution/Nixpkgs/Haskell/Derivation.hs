{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Distribution.Nixpkgs.Haskell.Derivation
  ( Derivation, pkgid, revision, src, isLibrary, isExecutable
  , extraFunctionArgs, libraryDepends, executableDepends, testDepends, configureFlags
  , cabalFlags, runHaddock, jailbreak, doCheck, testTarget, hyperlinkSource, enableSplitObjs
  , enableLibraryProfiling, enableExecutableProfiling, phaseOverrides, editedCabalFile, metaSection
  , dependencies
  )
  where

import Control.DeepSeq.Generics
import Data.List
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Set.Lens
import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.Haskell.OrphanInstances ( )
import Distribution.Nixpkgs.Meta
import Distribution.Nixpkgs.Haskell.BuildInfo
import Distribution.Package
import Distribution.PackageDescription ( FlagAssignment, FlagName(..) )
import GHC.Generics ( Generic )
import Language.Nix
import Control.Lens.Create
import Text.PrettyPrint.HughesPJClass
import Control.Lens
import Internal.PrettyPrinting

-- | A represtation of Nix expressions for building Haskell packages.
-- The data type correspond closely to the definition of
-- 'PackageDescription' from Cabal.

data Derivation = MkDerivation
  { _pkgid                      :: PackageIdentifier
  , _revision                   :: Int
  , _src                        :: DerivationSource
  , _isLibrary                  :: Bool
  , _isExecutable               :: Bool
  , _extraFunctionArgs          :: Set Identifier
  , _libraryDepends             :: BuildInfo
  , _executableDepends          :: BuildInfo
  , _testDepends                :: BuildInfo
  , _configureFlags             :: Set String
  , _cabalFlags                 :: FlagAssignment
  , _runHaddock                 :: Bool
  , _jailbreak                  :: Bool
  , _doCheck                    :: Bool
  , _testTarget                 :: String
  , _hyperlinkSource            :: Bool
  , _enableLibraryProfiling     :: Bool
  , _enableExecutableProfiling  :: Bool
  , _enableSplitObjs            :: Bool
  , _phaseOverrides             :: String
  , _editedCabalFile            :: String
  , _metaSection                :: Meta
  }
  deriving (Show, Eq, Generic)

instance Default Derivation where
  def = MkDerivation
    { _pkgid = error "undefined Derivation.pkgid"
    , _revision = error "undefined Derivation.revision"
    , _src = error "undefined Derivation.src"
    , _isLibrary = error "undefined Derivation.isLibrary"
    , _isExecutable = error "undefined Derivation.isExecutable"
    , _extraFunctionArgs = error "undefined Derivation.extraFunctionArgs"
    , _libraryDepends = error "undefined Derivation.libraryDepends"
    , _executableDepends = error "undefined Derivation.executableDepends"
    , _testDepends = error "undefined Derivation.testDepends"
    , _configureFlags = error "undefined Derivation.configureFlags"
    , _cabalFlags = error "undefined Derivation.cabalFlags"
    , _runHaddock = error "undefined Derivation.runHaddock"
    , _jailbreak = error "undefined Derivation.jailbreak"
    , _doCheck = error "undefined Derivation.doCheck"
    , _testTarget = error "undefined Derivation.testTarget"
    , _hyperlinkSource = error "undefined Derivation.hyperlinkSource"
    , _enableLibraryProfiling = error "undefined Derivation.enableLibraryProfiling"
    , _enableExecutableProfiling = error "undefined Derivation.enableExecutableProfiling"
    , _enableSplitObjs = error "undefined Derivation.enableSplitObjs"
    , _phaseOverrides = error "undefined Derivation.phaseOverrides"
    , _editedCabalFile = error "undefined Derivation.editedCabalFile"
    , _metaSection = error "undefined Derivation.metaSection"
    }

makeLenses ''Derivation

makeLensesFor [("_libraryDepends", "dependencies"), ("_executableDepends", "dependencies"), ("_testDepends", "dependencies")] ''Derivation

instance Package Derivation where
  packageId = view pkgid

instance NFData Derivation where rnf = genericRnf

instance Pretty Derivation where
  pPrint drv@(MkDerivation {..}) = funargs (map text ("mkDerivation" : toAscList inputs)) $$ vcat
    [ text "mkDerivation" <+> lbrace
    , nest 2 $ vcat
      [ attr "pname"   $ doubleQuotes $ disp (packageName _pkgid)
      , attr "version" $ doubleQuotes $ disp (packageVersion _pkgid)
      , sourceAttr _src
      , onlyIf (_revision > 0) $ attr "revision" $ doubleQuotes $ int _revision
      , onlyIf (not (null _editedCabalFile)) $ attr "editedCabalFile" $ string _editedCabalFile
      , listattr "configureFlags" empty (map (show . show) renderedFlags)
      , boolattr "isLibrary" (not _isLibrary || _isExecutable) _isLibrary
      , boolattr "isExecutable" (not _isLibrary || _isExecutable) _isExecutable
      , onlyIf (_libraryDepends /= mempty) $ pPrintBuildInfo "library" _libraryDepends
      , onlyIf (_executableDepends /= mempty) $ pPrintBuildInfo "executable" _executableDepends
      , onlyIf (_testDepends /= mempty) $ pPrintBuildInfo "test" _testDepends
      , boolattr "enableLibraryProfiling" _enableLibraryProfiling _enableLibraryProfiling
      , boolattr "enableExecutableProfiling" _enableExecutableProfiling _enableExecutableProfiling
      , boolattr "enableSplitObjs"  (not _enableSplitObjs) _enableSplitObjs
      , boolattr "doHaddock" (not _runHaddock) _runHaddock
      , boolattr "jailbreak" _jailbreak _jailbreak
      , boolattr "doCheck" (not _doCheck) _doCheck
      , onlyIf (not (null _testTarget)) $ attr "testTarget" $ string _testTarget
      , boolattr "hyperlinkSource" (not _hyperlinkSource) _hyperlinkSource
      , onlyIf (not (null _phaseOverrides)) $ vcat ((map text . lines) _phaseOverrides)
      , pPrint _metaSection
      ]
    , rbrace
    ]
    where
      inputs :: Set String
      inputs = Set.unions [ Set.map (view ident) _extraFunctionArgs
                          , setOf (dependencies . each . folded . localName . ident) drv
                          , Set.fromList ["fetch" ++ derivKind _src | derivKind _src /= "" && not isHackagePackage]
                          ]

      renderedFlags = [ text "-f" <> (if enable then empty else char '-') <> text f | (FlagName f, enable) <- _cabalFlags ]
                      ++ map text (toAscList _configureFlags)
      isHackagePackage = "mirror://hackage/" `isPrefixOf` derivUrl _src
      sourceAttr (DerivationSource{..})
        | isHackagePackage = attr "sha256" $ string derivHash
        | derivKind /= "" = vcat
           [ text "src" <+> equals <+> text ("fetch" ++ derivKind) <+> lbrace
           , nest 2 $ vcat
             [ attr "url" $ string derivUrl
             , attr "sha256" $ string derivHash
             , if derivRevision /= "" then attr "rev" (string derivRevision) else empty
             ]
           , rbrace <> semi
           ]
        | otherwise = attr "src" $ text derivUrl
