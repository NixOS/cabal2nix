{- |
   Module      :  Distribution.NixOS.Derivation.License
   Copyright   :  Peter Simons, Andres Loeh
   License     :  BSD3

   Maintainer  :  nix-dev@cs.uu.nl
   Stability   :  provisional
   Portability :  portable
-}

module Distribution.NixOS.Derivation.License ( License(..) ) where

import Distribution.NixOS.PrettyPrinting
import Distribution.Text

data License = Known String | Unknown (Maybe String)
  deriving (Show, Eq, Ord)

instance Text License where
  disp (Known x)   = text x
  disp (Unknown x) = string (maybe "unknown" id x)
  parse = error "parsing Distribution.NixOS.Derivation.License is not supported yet"

