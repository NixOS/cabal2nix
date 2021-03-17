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
fromCabalLicense (GPL (Just (versionNumbers -> [2])))      = Known "lib.licenses.gpl2Only"
fromCabalLicense (GPL (Just (versionNumbers -> [3])))      = Known "lib.licenses.gpl3Only"
fromCabalLicense (GPL (Just (versionNumbers -> [3,0])))    = Known "lib.licenses.gpl3Only"
fromCabalLicense (LGPL Nothing)                            = Unknown (Just "LGPL")
fromCabalLicense (LGPL (Just (versionNumbers -> [2,1])))   = Known "lib.licenses.lgpl21Only"
fromCabalLicense (LGPL (Just (versionNumbers -> [2])))     = Known "lib.licenses.lgpl2Only"
fromCabalLicense (LGPL (Just (versionNumbers -> [3])))     = Known "lib.licenses.lgpl3Only"
fromCabalLicense (LGPL (Just (versionNumbers -> [3,0])))   = Known "lib.licenses.lgpl3Only"
fromCabalLicense (AGPL Nothing)                            = Unknown (Just "AGPL")
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
fromCabalLicense OtherLicense                              = Unknown Nothing
fromCabalLicense (UnknownLicense "CC0-1.0")                = Known "lib.licenses.cc0"
fromCabalLicense (UnknownLicense "BSD3ClauseORApache20")   = Known "lib.licenses.bsd3"
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
            SPDX.AFL_2_1 -> Known "lib.licenses.afl21"
            SPDX.AFL_3_0 -> Known "lib.licenses.afl3"
            SPDX.AGPL_3_0_only -> Known "lib.licenses.agpl3Only"
            SPDX.AGPL_3_0_or_later -> Known "lib.licenses.agpl3Plus"
            SPDX.APSL_2_0 -> Known "lib.licenses.apsl20"
            SPDX.Artistic_1_0 -> Known "lib.licenses.artistic1"
            SPDX.Artistic_2_0 -> Known "lib.licenses.artistic2"
            SPDX.Apache_2_0 -> Known "lib.licenses.asl20"
            SPDX.BSL_1_0 -> Known "lib.licenses.boost"
            SPDX.Beerware -> Known "lib.licenses.beerware"
            SPDX.NullBSD -> Known "lib.licenses.bsd0"
            SPDX.BSD_2_Clause -> Known "lib.licenses.bsd2"
            SPDX.BSD_3_Clause -> Known "lib.licenses.bsd3"
            SPDX.BSD_4_Clause -> Known "lib.licenses.bsdOriginal"
            SPDX.ClArtistic -> Known "lib.licenses.clArtistic"
            SPDX.CC0_1_0 -> Known "lib.licenses.cc0"
            SPDX.CC_BY_NC_SA_2_0 -> Known "lib.licenses.cc-by-nc-sa-20"
            SPDX.CC_BY_NC_SA_2_5 -> Known "lib.licenses.cc-by-nc-sa-25"
            SPDX.CC_BY_NC_SA_3_0 -> Known "lib.licenses.cc-by-nc-sa-30"
            SPDX.CC_BY_NC_SA_4_0 -> Known "lib.licenses.cc-by-nc-sa-40"
            SPDX.CC_BY_NC_4_0 -> Known "lib.licenses.cc-by-nc-40"
            SPDX.CC_BY_ND_3_0 -> Known "lib.licenses.cc-by-nd-30"
            SPDX.CC_BY_SA_2_5 -> Known "lib.licenses.cc-by-sa-25"
            SPDX.CC_BY_3_0 -> Known "lib.licenses.cc-by-30"
            SPDX.CC_BY_SA_3_0 -> Known "lib.licenses.cc-by-sa-30"
            SPDX.CC_BY_4_0 -> Known "lib.licenses.cc-by-40"
            SPDX.CC_BY_SA_4_0 -> Known "lib.licenses.cc-by-sa-40"
            SPDX.CDDL_1_0 -> Known "lib.licenses.cddl"
            SPDX.CECILL_2_0 -> Known "lib.licenses.cecill20"
            SPDX.CECILL_B -> Known "lib.licenses.cecill-b"
            SPDX.CECILL_C -> Known "lib.licenses.cecill-c"
            SPDX.CPAL_1_0 -> Known "lib.licenses.cpal10"
            SPDX.CPL_1_0 -> Known "lib.licenses.cpl10"
            SPDX.Curl -> Known "lib.licenses.curl"
            SPDX.DOC -> Known "lib.licenses.doc"
            SPDX.EFL_1_0 -> Known "lib.licenses.efl10"
            SPDX.EFL_2_0 -> Known "lib.licenses.efl20"
            SPDX.EPL_1_0 -> Known "lib.licenses.epl10"
            SPDX.EPL_2_0 -> Known "lib.licenses.epl20"
            SPDX.EUPL_1_1 -> Known "lib.licenses.eupl11"
            SPDX.GFDL_1_2_only -> Known "lib.licenses.fdl12Only"
            SPDX.GFDL_1_3_only -> Known "lib.licenses.fdl13Only"
            SPDX.GPL_1_0_only -> Known "lib.licenses.gpl1Only"
            SPDX.GPL_1_0_or_later -> Known "lib.licenses.gpl1Plus"
            SPDX.GPL_2_0_only -> Known "lib.licenses.gpl2Only"
            SPDX.GPL_2_0_or_later -> Known "lib.licenses.gpl2Plus"
            SPDX.GPL_3_0_only -> Known "lib.licenses.gpl3Only"
            SPDX.GPL_3_0_or_later -> Known "lib.licenses.gpl3Plus"
            SPDX.HPND -> Known "lib.licenses.hpnd"
            SPDX.IJG -> Known "lib.licenses.ijg"
            SPDX.ImageMagick -> Known "lib.licenses.imagemagick"
            SPDX.IPA -> Known "lib.licenses.ipa"
            SPDX.IPL_1_0 -> Known "lib.licenses.ipl10"
            SPDX.ISC -> Known "lib.licenses.isc"
            SPDX.LGPL_2_0_only -> Known "lib.licenses.lgpl2Only"
            SPDX.LGPL_2_0_or_later -> Known "lib.licenses.lgpl2Plus"
            SPDX.LGPL_2_1_only -> Known "lib.licenses.lgpl21Only"
            SPDX.LGPL_2_1_or_later -> Known "lib.licenses.lgpl21Plus"
            SPDX.LGPL_3_0_only -> Known "lib.licenses.lgpl3Only"
            SPDX.LGPL_3_0_or_later -> Known "lib.licenses.lgpl3Plus"
            SPDX.Libpng -> Known "lib.licenses.libpng"
            SPDX.Libtiff -> Known "lib.licenses.libtiff"
            SPDX.LPPL_1_2 -> Known "lib.licenses.lppl12"
            SPDX.LPPL_1_3c -> Known "lib.licenses.lppl13c"
            SPDX.LPL_1_02 -> Known "lib.licenses.lpl-102"
            SPDX.MIT -> Known "lib.licenses.mit"
            SPDX.MPL_1_0 -> Known "lib.licenses.mpl10"
            SPDX.MPL_1_1 -> Known "lib.licenses.mpl11"
            SPDX.MPL_2_0 -> Known "lib.licenses.mpl20"
            SPDX.MS_PL -> Known "lib.licenses.mspl"
            SPDX.NCSA -> Known "lib.licenses.ncsa"
            SPDX.NPOSL_3_0 -> Known "lib.licenses.nposl3"
            SPDX.OFL_1_1 -> Known "lib.licenses.ofl"
            SPDX.OLDAP_2_8 -> Known "lib.licenses.openldap"
            SPDX.OpenSSL -> Known "lib.licenses.openssl"
            SPDX.OSL_2_1 -> Known "lib.licenses.osl21"
            SPDX.OSL_3_0 -> Known "lib.licenses.osl3"
            SPDX.PHP_3_01 -> Known "lib.licenses.php201"
            SPDX.PostgreSQL -> Known "lib.licenses.postgresql"
            SPDX.Python_2_0 -> Known "lib.licenses.psfl"
            SPDX.QPL_1_0 -> Known "lib.licenses.qpl"
            SPDX.Ruby -> Known "lib.licenses.ruby"
            SPDX.Sendmail -> Known "lib.licenses.sendmail"
            SPDX.SGI_B_2_0 -> Known "lib.licenses.sgi-b-0"
            SPDX.Sleepycat -> Known "lib.licenses.sleepycat"
            SPDX.TCL -> Known "lib.licenses.tcltx"
            SPDX.Unlicense -> Known "lib.licenses.unlicense"
            SPDX.Vim -> Known "lib.licenses.vim"
            SPDX.VSL_1_0 -> Known "lib.licenses.vsl10"
            SPDX.Watcom_1_0 -> Known "lib.licenses.watcom"
            SPDX.W3C -> Known "lib.licenses.w3c"
            SPDX.WTFPL -> Known "lib.licenses.wtfpl"
            SPDX.Zlib -> Known "lib.licenses.zlib"
            SPDX.ZPL_2_0 -> Known "lib.licenses.zpl20"
            SPDX.ZPL_2_1 -> Known "lib.licenses.zpl21"
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
isFreeLicense (Known "lib.licenses.unfree") = False
isFreeLicense (Unknown Nothing)             = False
isFreeLicense _                             = True
