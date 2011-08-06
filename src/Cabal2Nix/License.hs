module Cabal2Nix.License
  (module Cabal2Nix.License, module Distribution.License)
  where

import Distribution.License
import Distribution.Version

showLic :: License -> String
showLic (GPL Nothing)                     = show "GPL"
showLic (GPL (Just (Version [2] [])))     = "self.stdenv.lib.licenses.gpl2"
showLic (GPL (Just (Version [3] [])))     = "self.stdenv.lib.licenses.gpl3"
showLic (LGPL Nothing)                    = show "LGPL"
showLic (LGPL (Just (Version [2,1] [])))  = "self.stdenv.lib.licenses.lgpl21"
showLic (LGPL (Just (Version [3] [])))    = "self.stdenv.lib.licenses.lgpl3"
showLic BSD3                              = "self.stdenv.lib.licenses.bsd3"
showLic BSD4                              = "self.stdenv.lib.licenses.bsd4"
showLic MIT                               = "self.stdenv.lib.licenses.mit"
showLic PublicDomain                      = "self.stdenv.lib.licenses.publicDomain"
showLic AllRightsReserved                 = show "unknown"
showLic OtherLicense                      = show "unknown"
showLic l                                 = error $ "unknown license: " ++ show l
