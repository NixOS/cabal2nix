{-# LANGUAGE ViewPatterns #-}

module Distribution.Nixpkgs.Haskell.FromCabal.License
    ( fromCabalLicense
    , fromSPDXLicense
    , isFreeLicense
    ) where

import Data.List (intercalate)
import Distribution.License ( License(..), knownLicenses )
import Distribution.Nixpkgs.License
import qualified Distribution.Pretty as DPretty
import Language.Nix.PrettyPrinting (prettyShow)
import qualified Distribution.SPDX as SPDX
import Distribution.Text (display)
import Distribution.Version

-- TODO: Programmatically strip trailing zeros from license version numbers.

fromCabalLicense :: Distribution.License.License -> Distribution.Nixpkgs.License.License
fromCabalLicense (GPL Nothing)                             = Known "lib.licenses.gpl2Plus"
fromCabalLicense (GPL (Just (versionNumbers -> [2])))      = Known "lib.licenses.gpl2Only"
fromCabalLicense (GPL (Just (versionNumbers -> [3])))      = Known "lib.licenses.gpl3Only"
fromCabalLicense (GPL (Just (versionNumbers -> [3,0])))    = Known "lib.licenses.gpl3Only"
fromCabalLicense (LGPL Nothing)                            = Known "lib.licenses.lgpl2Plus"
fromCabalLicense (LGPL (Just (versionNumbers -> [2,1])))   = Known "lib.licenses.lgpl21Only"
fromCabalLicense (LGPL (Just (versionNumbers -> [2])))     = Known "lib.licenses.lgpl2Only"
fromCabalLicense (LGPL (Just (versionNumbers -> [3])))     = Known "lib.licenses.lgpl3Only"
fromCabalLicense (LGPL (Just (versionNumbers -> [3,0])))   = Known "lib.licenses.lgpl3Only"
fromCabalLicense (AGPL Nothing)                            = Known "lib.licenses.agpl3Plus"
fromCabalLicense (AGPL (Just (versionNumbers -> [3])))     = Known "lib.licenses.agpl3Only"
fromCabalLicense (AGPL (Just (versionNumbers -> [3,0])))   = Known "lib.licenses.agpl3Only"
fromCabalLicense (MPL (versionNumbers ->  [2,0]))          = Known "lib.licenses.mpl20"
fromCabalLicense BSD2                                      = Known "lib.licenses.bsd2"
fromCabalLicense BSD3                                      = Known "lib.licenses.bsd3"
fromCabalLicense BSD4                                      = Known "lib.licenses.bsdOriginal"
fromCabalLicense MIT                                       = Known "lib.licenses.mit"
fromCabalLicense PublicDomain                              = Known "lib.licenses.publicDomain"
fromCabalLicense UnspecifiedLicense                        = Known "lib.licenses.unfree"
fromCabalLicense AllRightsReserved                         = Known "lib.licenses.unfree"
fromCabalLicense (Apache Nothing)                          = Known "lib.licenses.asl20"
fromCabalLicense (Apache (Just (versionNumbers -> [2,0]))) = Known "lib.licenses.asl20"
fromCabalLicense ISC                                       = Known "lib.licenses.isc"
fromCabalLicense OtherLicense                              = Known "lib.licenses.free"
fromCabalLicense (UnknownLicense "CC0-1.0")                = Known "lib.licenses.cc0"
fromCabalLicense (UnknownLicense "BSD3ClauseORApache20")   = Known "lib.licenses.bsd3"
fromCabalLicense l                                         = error $ "Distribution.Nixpkgs.Haskell.FromCabal.License.fromCabalLicense: unknown license"
                                                                  ++ show l ++"\nChoose one of: " ++ intercalate ", " (map display knownLicenses)

fromSPDXLicenseExpression :: SPDX.LicenseExpression -> Distribution.Nixpkgs.License.License
fromSPDXLicenseExpression  (SPDX.ELicense simpl Nothing) =
  case simpl of
  SPDX.ELicenseId lid -> Known ("lib.licensesSpdx.\"" ++ DPretty.prettyShow lid ++ "\"")
  _ ->
    -- Not handed: the '+' suffix and user-defined licences references.
    -- Use the SPDX expression as a free-form license string.
    Unknown (Just $ DPretty.prettyShow simpl)
fromSPDXLicenseExpression (SPDX.ELicense simpl (Just excep)) =
  case simpl of
  SPDX.ELicenseId lid -> Known ("lib.licensesSpdx.\"" ++ DPretty.prettyShow lid ++ "\" lib.licensesSpdx.\"" ++  DPretty.prettyShow excep ++ "\"")
  _ ->
    -- Not handed: the '+' suffix and user-defined licences references.
    -- Use the SPDX expression as a free-form license string.
    Known ("\"" ++ DPretty.prettyShow simpl ++ "\" lib.licensesSpdx.\"" ++  DPretty.prettyShow excep ++ "\"")
fromSPDXLicenseExpression (SPDX.EAnd expres1 expres2) = Known (prettyShow (fromSPDXLicenseExpression expres1) ++ " " ++ prettyShow (fromSPDXLicenseExpression expres2))
fromSPDXLicenseExpression (SPDX.EOr expres1 expres2) = Known (prettyShow (fromSPDXLicenseExpression expres1) ++ " " ++ prettyShow (fromSPDXLicenseExpression expres2))

fromSPDXLicense :: SPDX.License -> Distribution.Nixpkgs.License.License
fromSPDXLicense SPDX.NONE = Known "lib.licenses.free"
fromSPDXLicense (SPDX.License expr) = Known ("[ " ++ prettyShow (fromSPDXLicenseExpression expr) ++ " ]")

-- "isFreeLicense" is used to determine whether we generate a "hydraPlatforms =
-- none" in the hackage2nix output for a package with the given license.

-- Note: If "isFreeLicense" returned false for a license which is not an unfree
-- license from "lib.licenses" the package would still be build by hydra if
-- another package depended on it.

-- Since all software on hackage needs to be "open source in spirit" and we
-- don‘t know any software on hackage for which we are not allowed to
-- distribute binary outputs we assume that a package has a free license if we
-- don‘t explicitly know otherwise.
isFreeLicense :: Distribution.Nixpkgs.License.License -> Bool
isFreeLicense (Known "lib.licenses.unfree") = False
isFreeLicense _                             = True
