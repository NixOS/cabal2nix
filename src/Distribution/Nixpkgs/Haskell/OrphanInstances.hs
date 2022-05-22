{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Nixpkgs.Haskell.OrphanInstances ( ) where

import Control.DeepSeq
import Data.Maybe
import Data.String
import qualified Data.Text as T
import Data.Yaml
import Distribution.Compiler
import Distribution.Package
import Distribution.Parsec
import Distribution.System
import Distribution.Types.PackageVersionConstraint
import Distribution.Pretty as Cabal
import qualified Data.Version as Base
import Distribution.Version
import Language.Nix.PrettyPrinting as Nix

instance NFData CompilerInfo
instance NFData AbiTag

instance IsString Version where
  fromString = text2isString "Version"

instance IsString VersionRange where
  fromString = text2isString "VersionRange"

instance IsString PackageIdentifier where
  fromString = text2isString "PackageIdentifier"

instance IsString PackageVersionConstraint where
  fromString = text2isString "PackageVersionConstraint"

instance IsString CompilerId where
  fromString = text2isString "CompilerId"

instance FromJSON Platform where
  parseJSON s =
    case s of
      (String "i686-linux") -> pure $ Platform I386 Linux
      (String "x86_64-linux") -> pure $ Platform X86_64 Linux
      (String "x86_64-darwin") -> pure $ Platform X86_64 OSX
      (String "aarch64-linux") -> pure $ Platform AArch64 Linux
      (String "aarch64-darwin") -> pure $ Platform AArch64 OSX
      (String "armv7l-linux") -> pure $ Platform (OtherArch "armv7l") Linux
      _ -> fail ("parseJSON: " ++ show s ++ " is not a valid platform")

instance FromJSON PackageName where
  parseJSON (String s) = return (fromString (T.unpack s))
  parseJSON s = fail ("parseJSON: " ++ show s ++ " is not a valid Haskell package name")

instance FromJSON PackageIdentifier where
  parseJSON (String s) = return (fromString (T.unpack s))
  parseJSON s = fail ("parseJSON: " ++ show s ++ " is not a valid Haskell package identifier")

instance FromJSON VersionRange where
  parseJSON (String s) = return (fromString (T.unpack s))
  parseJSON s = fail ("parseJSON: " ++ show s ++ " is not a valid Cabal VersionRange")

instance FromJSON PackageVersionConstraint where
  parseJSON (String s) = return (fromString (T.unpack s))
  parseJSON s = fail ("parseJSON: " ++ show s ++ " is not a valid Haskell PackageVersionConstraint")

instance FromJSON CompilerInfo where
  parseJSON (String s) = return (unknownCompilerInfo (fromString (T.unpack s)) NoAbiTag)
  parseJSON s = fail ("parseJSON: " ++ show s ++ " is not a valid Haskell compiler")

instance Nix.Pretty Version where
  pPrint = pretty

instance Nix.Pretty PackageName where
  pPrint = pretty

instance Nix.Pretty PackageIdentifier where
  pPrint = pretty

instance Nix.Pretty CompilerId where
  pPrint = pretty

instance Nix.Pretty Platform where
  pPrint = pretty

instance Nix.Pretty Base.Version where
  pPrint = text . Base.showVersion

-- parsing tools

text2isString :: Parsec a => String -> String -> a
text2isString t s = fromMaybe (error ("fromString: " ++ show s ++ " is not a valid " ++ t)) (simpleParsec s)
