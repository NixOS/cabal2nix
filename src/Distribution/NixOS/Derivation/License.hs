module Distribution.NixOS.Derivation.License
  ( License(..)
  , fromCabalLicense
  )
  where

import qualified Distribution.License as Cabal ( License(..) )
import Data.Version ( Version(..) )
import Distribution.Text
import Text.PrettyPrint

data License
  = GPL2
  | GPL3
  | LGPL21
  | LGPL3
  | BSD3
  | BSD4
  | MIT
  | PublicDomain
  | Proprietary
  | Unknown (Maybe String)
  deriving (Eq, Ord)

instance Show License where
  show GPL2                     = "self.stdenv.lib.licenses.gpl2"
  show GPL3                     = "self.stdenv.lib.licenses.gpl3"
  show LGPL21                   = "self.stdenv.lib.licenses.lgpl21"
  show LGPL3                    = "self.stdenv.lib.licenses.gpl3"
  show BSD3                     = "self.stdenv.lib.licenses.bsd3"
  show BSD4                     = "self.stdenv.lib.licenses.bsd4"
  show MIT                      = "self.stdenv.lib.licenses.mit"
  show PublicDomain             = "self.stdenv.lib.licenses.publicDomain"
  show Proprietary              = "self.stdenv.lib.licenses.proprietary"
  show (Unknown lic)            = show (maybe "unknown" id lic)

instance Text License where
  disp = text . show
  parse = error "parsing Distribution.NixOS.Derivation.License is not supported yet"

fromCabalLicense :: Cabal.License -> License
fromCabalLicense (Cabal.GPL Nothing)                     = Unknown (Just "GPL")
fromCabalLicense (Cabal.GPL (Just (Version [2] [])))     = GPL2
fromCabalLicense (Cabal.GPL (Just (Version [3] [])))     = GPL3
fromCabalLicense (Cabal.LGPL Nothing)                    = Unknown (Just "LGPL")
fromCabalLicense (Cabal.LGPL (Just (Version [2,1] [])))  = LGPL21
fromCabalLicense (Cabal.LGPL (Just (Version [3] [])))    = LGPL3
fromCabalLicense Cabal.BSD3                              = BSD3
fromCabalLicense Cabal.BSD4                              = BSD4
fromCabalLicense Cabal.MIT                               = MIT
fromCabalLicense Cabal.PublicDomain                      = PublicDomain
fromCabalLicense Cabal.AllRightsReserved                 = Proprietary
fromCabalLicense Cabal.OtherLicense                      = Unknown Nothing
fromCabalLicense l                                       = error $ "Distribution.NixOS.Derivation.License.fromCabalLicense: unknown license " ++ show l
