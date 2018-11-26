{-# LANGUAGE ViewPatterns #-}

module Distribution.Nixpkgs.Haskell.FromCabal.License
    ( fromCabalLicense
    , fromSPDXLicense
    , isFreeLicense
    ) where

import Data.List (intercalate)
import Distribution.License ( License(..), knownLicenses )
import Distribution.Nixpkgs.License
import Distribution.Pretty (prettyShow)
import qualified Distribution.SPDX as SPDX
import Distribution.Text (display)
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
fromCabalLicense (UnknownLicense "BSD3ClauseORApache20")   = Known "stdenv.lib.licenses.bsd3"
fromCabalLicense l                                         = error $ "Distribution.Nixpkgs.Haskell.FromCabal.License.fromCabalLicense: unknown license"
                                                                  ++ show l ++"\nChoose one of: " ++ intercalate ", " (map display knownLicenses)

fromSPDXLicense :: SPDX.License -> Distribution.Nixpkgs.License.License
fromSPDXLicense SPDX.NONE = Unknown Nothing
fromSPDXLicense (SPDX.License expr) =
  case expr of
    SPDX.ELicense simpl Nothing ->
      -- Not handled: license exceptions
      case simpl of
        SPDX.ELicenseId lid ->
          case lid of
            SPDX.AFL_2_1 -> Known "stdenv.lib.licenses.afl21"
            SPDX.AFL_3_0 -> Known "stdenv.lib.licenses.afl3"
            SPDX.AGPL_3_0_only -> Known "stdenv.lib.licenses.agpl3"
            SPDX.AGPL_3_0_or_later -> Known "stdenv.lib.licenses.agpl3Plus"
            SPDX.APSL_2_0 -> Known "stdenv.lib.licenses.apsl20"
            SPDX.Artistic_1_0 -> Known "stdenv.lib.licenses.artistic1"
            SPDX.Artistic_2_0 -> Known "stdenv.lib.licenses.artistic2"
            SPDX.Apache_2_0 -> Known "stdenv.lib.licenses.asl20"
            SPDX.BSL_1_0 -> Known "stdenv.lib.licenses.boost"
            SPDX.Beerware -> Known "stdenv.lib.licenses.beerware"
            SPDX.NullBSD -> Known "stdenv.lib.licenses.bsd0"
            SPDX.BSD_2_Clause -> Known "stdenv.lib.licenses.bsd2"
            SPDX.BSD_3_Clause -> Known "stdenv.lib.licenses.bsd3"
            SPDX.BSD_4_Clause -> Known "stdenv.lib.licenses.bsdOriginal"
            SPDX.ClArtistic -> Known "stdenv.lib.licenses.clArtistic"
            SPDX.CC0_1_0 -> Known "stdenv.lib.licenses.cc0"
            SPDX.CC_BY_NC_SA_2_0 -> Known "stdenv.lib.licenses.cc-by-nc-sa-20"
            SPDX.CC_BY_NC_SA_2_5 -> Known "stdenv.lib.licenses.cc-by-nc-sa-25"
            SPDX.CC_BY_NC_SA_3_0 -> Known "stdenv.lib.licenses.cc-by-nc-sa-30"
            SPDX.CC_BY_NC_SA_4_0 -> Known "stdenv.lib.licenses.cc-by-nc-sa-40"
            SPDX.CC_BY_NC_4_0 -> Known "stdenv.lib.licenses.cc-by-nc-40"
            SPDX.CC_BY_ND_3_0 -> Known "stdenv.lib.licenses.cc-by-nd-30"
            SPDX.CC_BY_SA_2_5 -> Known "stdenv.lib.licenses.cc-by-sa-25"
            SPDX.CC_BY_3_0 -> Known "stdenv.lib.licenses.cc-by-30"
            SPDX.CC_BY_SA_3_0 -> Known "stdenv.lib.licenses.cc-by-sa-30"
            SPDX.CC_BY_4_0 -> Known "stdenv.lib.licenses.cc-by-40"
            SPDX.CC_BY_SA_4_0 -> Known "stdenv.lib.licenses.cc-by-sa-40"
            SPDX.CDDL_1_0 -> Known "stdenv.lib.licenses.cddl"
            SPDX.CECILL_2_0 -> Known "stdenv.lib.licenses.cecill20"
            SPDX.CECILL_B -> Known "stdenv.lib.licenses.cecill-b"
            SPDX.CECILL_C -> Known "stdenv.lib.licenses.cecill-c"
            SPDX.CPAL_1_0 -> Known "stdenv.lib.licenses.cpal10"
            SPDX.CPL_1_0 -> Known "stdenv.lib.licenses.cpl10"
            SPDX.Curl -> Known "stdenv.lib.licenses.curl"
            SPDX.DOC -> Known "stdenv.lib.licenses.doc"
            SPDX.EFL_1_0 -> Known "stdenv.lib.licenses.efl10"
            SPDX.EFL_2_0 -> Known "stdenv.lib.licenses.efl20"
            SPDX.EPL_1_0 -> Known "stdenv.lib.licenses.epl10"
            SPDX.EPL_2_0 -> Known "stdenv.lib.licenses.epl20"
            SPDX.EUPL_1_1 -> Known "stdenv.lib.licenses.eupl11"
            SPDX.GFDL_1_2_only -> Known "stdenv.lib.licenses.fdl12"
            SPDX.GFDL_1_3_only -> Known "stdenv.lib.licenses.fdl13"
            SPDX.GPL_1_0_only -> Known "stdenv.lib.licenses.gpl1"
            SPDX.GPL_1_0_or_later -> Known "stdenv.lib.licenses.gpl1Plus"
            SPDX.GPL_2_0_only -> Known "stdenv.lib.licenses.gpl2"
            SPDX.GPL_2_0_or_later -> Known "stdenv.lib.licenses.gpl2Plus"
            SPDX.GPL_3_0_only -> Known "stdenv.lib.licenses.gpl3"
            SPDX.GPL_3_0_or_later -> Known "stdenv.lib.licenses.gpl3Plus"
            SPDX.HPND -> Known "stdenv.lib.licenses.hpnd"
            SPDX.IJG -> Known "stdenv.lib.licenses.ijg"
            SPDX.ImageMagick -> Known "stdenv.lib.licenses.imagemagick"
            SPDX.IPA -> Known "stdenv.lib.licenses.ipa"
            SPDX.IPL_1_0 -> Known "stdenv.lib.licenses.ipl10"
            SPDX.ISC -> Known "stdenv.lib.licenses.isc"
            SPDX.JasPer_2_0 -> Known "stdenv.lib.licenses.jasper"
            SPDX.LGPL_2_0_only -> Known "stdenv.lib.licenses.lgpl2"
            SPDX.LGPL_2_0_or_later -> Known "stdenv.lib.licenses.lgpl2Plus"
            SPDX.LGPL_2_1_only -> Known "stdenv.lib.licenses.lgpl21"
            SPDX.LGPL_2_1_or_later -> Known "stdenv.lib.licenses.lgpl21Plus"
            SPDX.LGPL_3_0_only -> Known "stdenv.lib.licenses.lgpl3"
            SPDX.LGPL_3_0_or_later -> Known "stdenv.lib.licenses.lgpl3Plus"
            SPDX.Libpng -> Known "stdenv.lib.licenses.libpng"
            SPDX.Libtiff -> Known "stdenv.lib.licenses.libtiff"
            SPDX.LPPL_1_2 -> Known "stdenv.lib.licenses.lppl12"
            SPDX.LPPL_1_3c -> Known "stdenv.lib.licenses.lppl13c"
            SPDX.LPL_1_02 -> Known "stdenv.lib.licenses.lpl-102"
            SPDX.MIT -> Known "stdenv.lib.licenses.mit"
            SPDX.MPL_1_0 -> Known "stdenv.lib.licenses.mpl10"
            SPDX.MPL_1_1 -> Known "stdenv.lib.licenses.mpl11"
            SPDX.MPL_2_0 -> Known "stdenv.lib.licenses.mpl20"
            SPDX.MS_PL -> Known "stdenv.lib.licenses.mspl"
            SPDX.NCSA -> Known "stdenv.lib.licenses.ncsa"
            SPDX.NPOSL_3_0 -> Known "stdenv.lib.licenses.nposl3"
            SPDX.OFL_1_1 -> Known "stdenv.lib.licenses.ofl"
            SPDX.OLDAP_2_8 -> Known "stdenv.lib.licenses.openldap"
            SPDX.OpenSSL -> Known "stdenv.lib.licenses.openssl"
            SPDX.OSL_2_1 -> Known "stdenv.lib.licenses.osl21"
            SPDX.OSL_3_0 -> Known "stdenv.lib.licenses.osl3"
            SPDX.PHP_3_01 -> Known "stdenv.lib.licenses.php201"
            SPDX.PostgreSQL -> Known "stdenv.lib.licenses.postgresql"
            SPDX.Python_2_0 -> Known "stdenv.lib.licenses.psfl"
            SPDX.QPL_1_0 -> Known "stdenv.lib.licenses.qpl"
            SPDX.Ruby -> Known "stdenv.lib.licenses.ruby"
            SPDX.Sendmail -> Known "stdenv.lib.licenses.sendmail"
            SPDX.SGI_B_2_0 -> Known "stdenv.lib.licenses.sgi-b-0"
            SPDX.Sleepycat -> Known "stdenv.lib.licenses.sleepycat"
            SPDX.TCL -> Known "stdenv.lib.licenses.tcltx"
            SPDX.Unlicense -> Known "stdenv.lib.licenses.unlicense"
            SPDX.Vim -> Known "stdenv.lib.licenses.vim"
            SPDX.VSL_1_0 -> Known "stdenv.lib.licenses.vsl10"
            SPDX.Watcom_1_0 -> Known "stdenv.lib.licenses.watcom"
            SPDX.W3C -> Known "stdenv.lib.licenses.w3c"
            SPDX.WTFPL -> Known "stdenv.lib.licenses.wtfpl"
            SPDX.Zlib -> Known "stdenv.lib.licenses.zlib"
            SPDX.ZPL_2_0 -> Known "stdenv.lib.licenses.zpl20"
            SPDX.ZPL_2_1 -> Known "stdenv.lib.licenses.zpl21"
            _ ->
              -- Licence is not in Nixpkgs.
              -- Use the SPDX expression as a free-form license string.
              Unknown (Just $ prettyShow expr)
        _ ->
          -- Not handed: the '+' suffix and user-defined licences references.
          -- Use the SPDX expression as a free-form license string.
          Unknown (Just $ prettyShow expr)
    _ ->
      -- Not handled: compound expressions, not expressible in Nixpkgs.
      -- Use the SPDX expression as a free-form license string.
      Unknown (Just $ prettyShow expr)

isFreeLicense :: Distribution.Nixpkgs.License.License -> Bool
isFreeLicense (Known "stdenv.lib.licenses.unfree") = False
isFreeLicense (Unknown Nothing)                    = False
isFreeLicense _                                    = True
