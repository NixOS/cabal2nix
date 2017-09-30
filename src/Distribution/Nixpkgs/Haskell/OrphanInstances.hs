{-# OPTIONS_GHC -fno-warn-orphans #-}

module Distribution.Nixpkgs.Haskell.OrphanInstances ( ) where

import Control.DeepSeq
import Data.Maybe
import Data.String
import qualified Data.Text as T
import Data.Yaml
import Distribution.Compiler
import Distribution.License
import Distribution.Package
import Distribution.PackageDescription
import Distribution.System
import Distribution.Text
import Distribution.Types.CondTree
import Distribution.Types.ExecutableScope
import Distribution.Types.ForeignLib
import Distribution.Types.ForeignLibOption
import Distribution.Types.ForeignLibType
import Distribution.Types.IncludeRenaming
import Distribution.Types.Mixin
import Distribution.Version
import Language.Haskell.Extension

instance NFData SetupBuildInfo
instance (NFData v, NFData c, NFData a) => NFData (CondTree v c a)
instance (NFData v, NFData c, NFData a) => NFData (CondBranch v c a)
instance NFData Arch
instance NFData Benchmark
instance NFData BenchmarkInterface
instance NFData BenchmarkType
instance NFData BuildInfo
instance NFData BuildType
instance NFData LibVersionInfo
instance NFData CompilerFlavor
instance NFData ConfVar
instance NFData ExecutableScope
instance NFData IncludeRenaming
instance NFData ForeignLibType
instance NFData ForeignLibOption
instance NFData Executable
instance NFData Extension
instance NFData Flag
instance NFData Mixin
instance NFData FlagName
instance NFData GenericPackageDescription
instance NFData KnownExtension
instance NFData Language
instance NFData Library
instance NFData ForeignLib
instance NFData License
instance NFData ModuleReexport
instance NFData ModuleRenaming
instance NFData OS
instance NFData PackageDescription
instance NFData RepoKind
instance NFData RepoType
instance NFData SourceRepo
instance NFData TestSuite
instance NFData TestSuiteInterface
instance NFData TestType
instance NFData a => NFData (Condition a)
instance NFData Platform
instance NFData CompilerInfo
instance NFData CompilerId
instance NFData AbiTag

instance IsString Version where
  fromString = text2isString "Version"

instance IsString VersionRange where
  fromString = text2isString "VersionRange"

instance IsString PackageIdentifier where
  fromString = text2isString "PackageIdentifier"

instance IsString Dependency where
  fromString = text2isString "Dependency"

instance IsString CompilerId where
  fromString = text2isString "CompilerId"

instance IsString Platform where
  fromString "i686-linux" = Platform I386 Linux
  fromString "x86_64-linux" = Platform X86_64 Linux
  fromString "x86_64-darwin" = Platform X86_64 OSX
  fromString s = error ("fromString: " ++ show s ++ " is not a valid platform")

instance FromJSON Platform where
  parseJSON (String s) = pure (fromString (T.unpack s))
  parseJSON s = fail ("parseJSON: " ++ show s ++ " is not a valid platform")

instance FromJSON PackageName where
  parseJSON (String s) = return (fromString (T.unpack s))
  parseJSON s = fail ("parseJSON: " ++ show s ++ " is not a valid Haskell package name")

instance FromJSON PackageIdentifier where
  parseJSON (String s) = return (fromString (T.unpack s))
  parseJSON s = fail ("parseJSON: " ++ show s ++ " is not a valid Haskell package identifier")

instance FromJSON Dependency where
  parseJSON (String s) = return (fromString (T.unpack s))
  parseJSON s = fail ("parseJSON: " ++ show s ++ " is not a valid Haskell Dependency")

instance FromJSON CompilerInfo where
  parseJSON (String s) = return (unknownCompilerInfo (fromString (T.unpack s)) NoAbiTag)
  parseJSON s = fail ("parseJSON: " ++ show s ++ " is not a valid Haskell compiler")

-- parsing tools

text2isString :: Text a => String -> String -> a
text2isString t s = fromMaybe (error ("fromString: " ++ show s ++ " is not a valid " ++ t)) (simpleParse s)
