{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}           -- for FlagName below

module Distribution.Nixpkgs.Haskell
  ( Derivation, nullDerivation, pkgid, revision, src, isLibrary, isExecutable
  , extraFunctionArgs, libraryDepends, executableDepends, testDepends, configureFlags
  , cabalFlags, runHaddock, jailbreak, doCheck, testTarget, hyperlinkSource, enableSplitObjs
  , phaseOverrides, editedCabalFile, metaSection
  , BuildInfo, haskell, pkgconfig, system, tool
  , dependencies, anyDep
  , dep, unDep
  )
  where

import Control.DeepSeq.Generics
import Control.Lens
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Function
import Data.List
import Distribution.Version
import Distribution.Nixpkgs.Meta
import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.Util.PrettyPrinting
import Distribution.Package
import Distribution.PackageDescription ( FlagAssignment, FlagName(..) )
import GHC.Generics ( Generic )

data BuildInfo = BuildInfo
  { _haskell :: Set Dependency
  , _pkgconfig :: Set Dependency
  , _system :: Set Dependency
  , _tool :: Set Dependency
  }
  deriving (Show, Eq, Generic)

makeLenses ''BuildInfo

makeLensesFor [("_haskell", "anyDep"), ("_pkgconfig", "anyDep"), ("_system", "anyDep"), ("_tool", "anyDep")] ''BuildInfo

instance Monoid BuildInfo where
  mempty = BuildInfo mempty mempty mempty mempty
  BuildInfo w1 x1 y1 z1 `mappend` BuildInfo w2 x2 y2 z2 = BuildInfo (w1 `mappend` w2) (x1 `mappend` x2) (y1 `mappend` y2) (z1 `mappend` z2)

-- | A represtation of Nix expressions for building Haskell packages.
-- The data type correspond closely to the definition of
-- 'PackageDescription' from Cabal.
--
-- Note that the "Text" instance definition provides pretty-printing,
-- but no parsing as of now!
data Derivation = MkDerivation
  { _pkgid               :: PackageIdentifier
  , _revision            :: Int
  , _src                 :: DerivationSource
  , _isLibrary           :: Bool
  , _isExecutable        :: Bool
  , _extraFunctionArgs   :: Set String
  , _libraryDepends      :: BuildInfo
  , _executableDepends   :: BuildInfo
  , _testDepends         :: BuildInfo
  , _configureFlags      :: Set String
  , _cabalFlags          :: FlagAssignment
  , _runHaddock          :: Bool
  , _jailbreak           :: Bool
  , _doCheck             :: Bool
  , _testTarget          :: String
  , _hyperlinkSource     :: Bool
  , _enableSplitObjs     :: Bool
  , _phaseOverrides      :: String
  , _editedCabalFile     :: String
  , _metaSection         :: Meta
  }
  deriving (Show, Eq, Generic)

nullDerivation :: Derivation
nullDerivation = MkDerivation
  { _pkgid = error "undefined _pkgid"
  , _revision = error "undefined _revision"
  , _src = error "undefined _src"
  , _isLibrary = error "undefined _isLibrary"
  , _isExecutable = error "undefined _isExecutable"
  , _extraFunctionArgs = error "undefined _extraFunctionArgs"
  , _libraryDepends = error "undefined _libraryDepends"
  , _executableDepends = error "undefined _executableDepends"
  , _testDepends = error "undefined _testDepends"
  , _configureFlags = error "undefined _configureFlags"
  , _cabalFlags = error "undefined _cabalFlags"
  , _runHaddock = error "undefined _runHaddock"
  , _jailbreak = error "undefined _jailbreak"
  , _doCheck = error "undefined _doCheck"
  , _testTarget = error "undefined _testTarget"
  , _hyperlinkSource = error "undefined _hyperlinkSource"
  , _enableSplitObjs = error "undefined _enableSplitObjs"
  , _phaseOverrides = error "undefined _phaseOverrides"
  , _editedCabalFile = error "undefined _editedCabalFile"
  , _metaSection = error "undefined _metaSection"
  }

makeLenses ''Derivation

makeLensesFor [("_libraryDepends", "dependencies"), ("_executableDepends", "dependencies"), ("_testDepends", "dependencies")] ''Derivation

instance Package Derivation where
  packageId = view pkgid

instance NFData BuildInfo where rnf = genericRnf

instance NFData Derivation where rnf = genericRnf

instance NFData FlagName where rnf = genericRnf

instance NFData VersionRange where rnf = genericRnf

instance NFData Dependency where rnf = genericRnf

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
      inputs = Set.unions [ _extraFunctionArgs
                          , view (dependencies . anyDep . to (Set.map unDep)) drv
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

pPrintBuildInfo :: String -> BuildInfo -> Doc
pPrintBuildInfo prefix bi = vcat
  [ setattr (prefix++"HaskellDepends") (Set.map unDep (bi^.haskell))
  , setattr (prefix++"SystemDepends")  (Set.map unDep (bi^.system))
  , setattr (prefix++"PkgconfigDepends") (Set.map unDep (bi^.pkgconfig))
  , setattr (prefix++"ToolDepends") (Set.map unDep (bi^.tool))
  ]

dep :: String -> Dependency
dep s = Dependency (PackageName s) anyVersion

unDep :: Dependency -> String
unDep (Dependency (PackageName x) _) = x

instance Ord Dependency where
  compare = compare `on` unDep
