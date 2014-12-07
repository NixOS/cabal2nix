{- |
   Module      :  Distribution.Hackage.DB
   License     :  BSD3
   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  portable

   This module provides simple access to the Hackage database by means
   of 'Map'.
 -}

module Distribution.Hackage.DB
  ( Hackage, readHackage, readHackage', parseHackage, hackagePath
  , module Data.Map
  , module Data.Version
  , module Distribution.Package
  , module Distribution.PackageDescription
  )
  where

import Data.Map
import Data.Version
import Distribution.Hackage.DB.Parsed
import Distribution.Hackage.DB.Path
import Distribution.Package
import Distribution.PackageDescription
