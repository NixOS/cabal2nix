{- |
   Maintainer:  simons@cryp.to
   Stability:   provisional
   Portability: portable
 -}

module Distribution.Hackage.DB.Errors where

import Codec.Archive.Tar
import Control.Exception
import Data.Typeable ( Typeable )

data IncorrectTarfile = IncorrectTarfile FilePath FormatError deriving (Show, Typeable)
instance Exception IncorrectTarfile

data UnsupportedTarEntry = UnsupportedTarEntry FilePath Entry deriving (Show, Typeable)
instance Exception UnsupportedTarEntry
