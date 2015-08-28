{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Distribution.Nixpkgs.Haskell.OrphanInstances ( ) where

import Control.DeepSeq.Generics
import Data.Maybe
import Data.String
import Distribution.Compiler
import Distribution.License
import Distribution.ModuleName hiding ( main )
import Distribution.Package
import Distribution.PackageDescription
import Distribution.System
import Distribution.Text
import Distribution.Version
import GHC.Generics ( Generic )
import Language.Haskell.Extension

deriving instance Generic (CondTree v c a)
deriving instance Generic (Condition a)
deriving instance Generic ConfVar
deriving instance Generic Flag
deriving instance Generic GenericPackageDescription

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

instance IsString PackageName where
  fromString = text2isString "PackageName"

instance IsString PackageIdentifier where
  fromString = text2isString "PackageIdentifier"

instance IsString Dependency where
  fromString = text2isString "Dependency"

text2isString :: Text a => String -> String -> a
text2isString t s = fromMaybe (error ("fromString: " ++ show s ++ " is not a valid " ++ t)) (simpleParse s)
