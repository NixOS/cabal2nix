module Cabal2Nix.License ( fromCabalLicense ) where

import Distribution.NixOS.Derivation.License
import Distribution.License ( License(..) )
import Data.Version

fromCabalLicense :: Distribution.License.License -> Distribution.NixOS.Derivation.License.License
fromCabalLicense (GPL Nothing)                     = Unknown (Just "GPL")
fromCabalLicense (GPL (Just (Version [2] [])))     = Known "self.stdenv.lib.licenses.gpl2"
fromCabalLicense (GPL (Just (Version [3] [])))     = Known "self.stdenv.lib.licenses.gpl3"
fromCabalLicense (LGPL Nothing)                    = Unknown (Just "LGPL")
fromCabalLicense (LGPL (Just (Version [2,1] [])))  = Known "self.stdenv.lib.licenses.lgpl21"
fromCabalLicense (LGPL (Just (Version [2] [])))    = Known "self.stdenv.lib.licenses.lgpl2"
fromCabalLicense (LGPL (Just (Version [3] [])))    = Known "self.stdenv.lib.licenses.gpl3"
fromCabalLicense BSD3                              = Known "self.stdenv.lib.licenses.bsd3"
fromCabalLicense BSD4                              = Known "self.stdenv.lib.licenses.bsd4"
fromCabalLicense MIT                               = Known "self.stdenv.lib.licenses.mit"
fromCabalLicense PublicDomain                      = Known "self.stdenv.lib.licenses.publicDomain"
fromCabalLicense AllRightsReserved                 = Known "self.stdenv.lib.licenses.unfree"
fromCabalLicense (Apache Nothing)                  = Known "self.stdenv.lib.licenses.asl20"
fromCabalLicense (Apache (Just (Version [2,0] [])))= Known "self.stdenv.lib.licenses.asl20"
fromCabalLicense OtherLicense                      = Unknown Nothing
fromCabalLicense l                                 = error $ "Cabal2Nix.License.fromCabalLicense: unknown license " ++ show l
