{-# LANGUAGE DeriveDataTypeable #-}

{- |
   Maintainer:  simons@cryp.to
   Stability:   provisional
   Portability: portable
 -}

module Distribution.Hackage.DB.Errors where

import Codec.Archive.Tar
import Control.Exception
import Data.Typeable ( Typeable )
import Distribution.Package
import Distribution.Version

data HackageDBTarball a = HackageDBTarball FilePath a deriving (Show, Typeable)
instance (Show a, Typeable a) => Exception (HackageDBTarball a)

data HackageDBPackageName a = HackageDBPackageName PackageName a deriving (Show, Typeable)
instance (Show a, Typeable a) => Exception (HackageDBPackageName a)

data HackageDBPackageVersion a = HackageDBPackageVersion Version a deriving (Show, Typeable)
instance (Show a, Typeable a) => Exception (HackageDBPackageVersion a)

newtype IncorrectTarfile = IncorrectTarfile FormatError deriving (Show, Typeable)
instance Exception IncorrectTarfile

newtype UnsupportedTarEntry = UnsupportedTarEntry Entry deriving (Show, Typeable)
instance Exception UnsupportedTarEntry

newtype InvalidMetaFile = InvalidMetaFile String deriving (Show, Typeable)
instance Exception InvalidMetaFile
