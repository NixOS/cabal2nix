{-# LANGUAGE ViewPatterns #-}

module Distribution.Nixpkgs.Haskell.FromCabal.License ( fromCabalLicense ) where

import Distribution.Nixpkgs.License
import Distribution.License ( License(..), knownLicenses )
import Distribution.Text (display)
import Data.List (intercalate)
import Distribution.Version

-- TODO: Programmatically strip trailing zeros from license version numbers.

fromCabalLicense :: Distribution.License.License -> Distribution.Nixpkgs.License.License
fromCabalLicense (GPL Nothing)                             = Unknown (Just "GPL")
fromCabalLicense (GPL (Just (versionNumbers -> [2])))      = Known "stdenv.lib.licenses.gpl2"
fromCabalLicense (GPL (Just (versionNumbers -> [3])))      = Known "stdenv.lib.licenses.gpl3"
fromCabalLicense (GPL (Just (versionNumbers -> [3,0])))    = Known "stdenv.lib.licenses.gpl3"
fromCabalLicense (LGPL Nothing)                            = Unknown (Just "LGPL")
fromCabalLicense (LGPL (Just (versionNumbers -> [2,1])))   = Known "stdenv.lib.licenses.lgpl21"
fromCabalLicense (LGPL (Just (versionNumbers -> [2])))     = Known "stdenv.lib.licenses.lgpl2"
fromCabalLicense (LGPL (Just (versionNumbers -> [3])))     = Known "stdenv.lib.licenses.lgpl3"
fromCabalLicense (LGPL (Just (versionNumbers -> [3,0])))   = Known "stdenv.lib.licenses.lgpl3"
fromCabalLicense (AGPL Nothing)                            = Unknown (Just "AGPL")
fromCabalLicense (AGPL (Just (versionNumbers -> [3])))     = Known "stdenv.lib.licenses.agpl3"
fromCabalLicense (AGPL (Just (versionNumbers -> [3,0])))   = Known "stdenv.lib.licenses.agpl3"
fromCabalLicense (MPL (versionNumbers ->  [2,0]))          = Known "stdenv.lib.licenses.mpl20"
fromCabalLicense BSD2                                      = Known "stdenv.lib.licenses.bsd2"
fromCabalLicense BSD3                                      = Known "stdenv.lib.licenses.bsd3"
fromCabalLicense BSD4                                      = Known "stdenv.lib.licenses.bsdOriginal"
fromCabalLicense MIT                                       = Known "stdenv.lib.licenses.mit"
fromCabalLicense PublicDomain                              = Known "stdenv.lib.licenses.publicDomain"
fromCabalLicense UnspecifiedLicense                        = Known "stdenv.lib.licenses.unfree"
fromCabalLicense AllRightsReserved                         = Known "stdenv.lib.licenses.unfree"
fromCabalLicense (Apache Nothing)                          = Known "stdenv.lib.licenses.asl20"
fromCabalLicense (Apache (Just (versionNumbers -> [2,0]))) = Known "stdenv.lib.licenses.asl20"
fromCabalLicense ISC                                       = Known "stdenv.lib.licenses.isc"
fromCabalLicense OtherLicense                              = Unknown Nothing
fromCabalLicense (UnknownLicense "CC0-1.0")                = Known "stdenv.lib.licenses.cc0"
fromCabalLicense l                                         = error $ "Distribution.Nixpkgs.Haskell.FromCabal.License.fromCabalLicense: unknown license"
                                                                  ++ show l ++"\nChoose one of: " ++ intercalate ", " (map display knownLicenses)
