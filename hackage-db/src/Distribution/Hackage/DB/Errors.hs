{-# LANGUAGE DeriveAnyClass #-}

{- |
   Maintainer:  simons@cryp.to
   Stability:   provisional
   Portability: portable
 -}

module Distribution.Hackage.DB.Errors where

import Codec.Archive.Tar
import Control.Exception
import Distribution.Package
import Distribution.Version

data HackageDBTarball a = HackageDBTarball FilePath a deriving (Show, Exception)

data HackageDBPackageName a = HackageDBPackageName PackageName a deriving (Show, Exception)

data HackageDBPackageVersion a = HackageDBPackageVersion Version a deriving (Show, Exception)

newtype IncorrectTarfile = IncorrectTarfile FormatError deriving (Show, Exception)

newtype UnsupportedTarEntry = UnsupportedTarEntry Entry deriving (Show, Exception)

newtype InvalidMetaFile = InvalidMetaFile String deriving (Show, Exception)

newtype InvalidCabalFile = InvalidCabalFile String deriving (Show, Exception)

data InvalidRepresentationOfType = InvalidRepresentationOfType String String deriving (Show, Exception)

data NoHackageTarballFound = NoHackageTarballFound deriving (Show, Exception)
