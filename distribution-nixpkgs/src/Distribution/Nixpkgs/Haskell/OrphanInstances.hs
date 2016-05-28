{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Distribution.Nixpkgs.Haskell.OrphanInstances ( ) where

import Control.DeepSeq
import Data.Maybe
import Data.String
import qualified Data.Text as T
import Data.Yaml
import Distribution.Compiler
import Distribution.License
import Distribution.ModuleName hiding ( main, fromString )
import Distribution.Package
import Distribution.PackageDescription
import Distribution.System
import Distribution.Text
import Distribution.Version
import Language.Haskell.Extension

#if !MIN_VERSION_Cabal(1,23,0)
import GHC.Generics ( Generic )
deriving instance Generic (CondTree v c a)
deriving instance Generic (Condition a)
deriving instance Generic ConfVar
deriving instance Generic Flag
deriving instance Generic GenericPackageDescription
#else
instance NFData SetupBuildInfo
#endif

instance (NFData v, NFData c, NFData a) => NFData (CondTree v c a)
instance NFData Arch
instance NFData Benchmark
instance NFData BenchmarkInterface
instance NFData BenchmarkType
instance NFData BuildInfo
instance NFData BuildType
instance NFData CompilerFlavor
instance NFData ConfVar
instance NFData Dependency
instance NFData Executable
instance NFData Extension
instance NFData Flag
instance NFData FlagName
instance NFData GenericPackageDescription
instance NFData KnownExtension
instance NFData Language
instance NFData Library
instance NFData License
instance NFData ModuleName
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
instance NFData VersionRange
instance NFData a => NFData (Condition a)
instance NFData Platform
instance NFData CompilerInfo
instance NFData CompilerId
instance NFData AbiTag

instance IsString PackageName where
  fromString = text2isString "PackageName"

instance IsString Version where
  fromString = text2isString "Version"

instance IsString PackageIdentifier where
  fromString = text2isString "PackageIdentifier"

instance IsString Dependency where
  fromString = text2isString "Dependency"

instance IsString CompilerId where
  fromString = text2isString "CompilerId"

instance IsString Platform where
  fromString "i686-linux" = Platform I386 Linux
  fromString "x86_64-linux" = Platform X86_64 Linux
  fromString "x86_64-darwin" = Platform X86_64 (OtherOS "darwin")
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
