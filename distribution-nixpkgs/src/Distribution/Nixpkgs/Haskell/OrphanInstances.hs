{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Distribution.Nixpkgs.Haskell.OrphanInstances ( ) where

import Control.DeepSeq.Generics
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
instance NFData SetupBuildInfo where rnf = genericRnf
#endif

instance (NFData v, NFData c, NFData a) => NFData (CondTree v c a) where rnf = genericRnf
instance NFData Arch where rnf = genericRnf
instance NFData Benchmark where rnf = genericRnf
instance NFData BenchmarkInterface where rnf = genericRnf
instance NFData BenchmarkType where rnf = genericRnf
instance NFData BuildInfo where rnf = genericRnf
instance NFData BuildType where rnf = genericRnf
instance NFData CompilerFlavor where rnf = genericRnf
instance NFData ConfVar where rnf = genericRnf
instance NFData Dependency where rnf = genericRnf
instance NFData Executable where rnf = genericRnf
instance NFData Extension where rnf = genericRnf
instance NFData Flag where rnf = genericRnf
instance NFData FlagName where rnf = genericRnf
instance NFData GenericPackageDescription where rnf = genericRnf
instance NFData KnownExtension where rnf = genericRnf
instance NFData Language where rnf = genericRnf
instance NFData Library where rnf = genericRnf
instance NFData License where rnf = genericRnf
instance NFData ModuleName where rnf = genericRnf
instance NFData ModuleReexport where rnf = genericRnf
instance NFData ModuleRenaming where rnf = genericRnf
instance NFData OS where rnf = genericRnf
instance NFData PackageDescription where rnf = genericRnf
instance NFData RepoKind where rnf = genericRnf
instance NFData RepoType where rnf = genericRnf
instance NFData SourceRepo where rnf = genericRnf
instance NFData TestSuite where rnf = genericRnf
instance NFData TestSuiteInterface where rnf = genericRnf
instance NFData TestType where rnf = genericRnf
instance NFData VersionRange where rnf = genericRnf
instance NFData a => NFData (Condition a) where rnf = genericRnf
instance NFData Platform where rnf = genericRnf
instance NFData CompilerInfo where rnf = genericRnf
instance NFData CompilerId where rnf = genericRnf
instance NFData AbiTag where rnf = genericRnf

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
