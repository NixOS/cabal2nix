{-# OPTIONS_GHC -fno-warn-orphans #-}

module Distribution.Nixpkgs.Haskell.OrphanInstances ( ) where

import Control.DeepSeq
import Data.Maybe
import Data.String
import qualified Data.Text as T
import Data.Yaml
import Distribution.Compiler
import Distribution.Package
import Distribution.System
import Distribution.Text
import Distribution.Version

instance NFData CompilerInfo
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
