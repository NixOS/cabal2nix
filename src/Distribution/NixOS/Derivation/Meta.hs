{-# LANGUAGE DeriveGeneric #-}
{- |
   Module      :  Distribution.NixOS.Derivation.Meta
   License     :  BSD3

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  portable

   A representation of the @meta@ section used in Nix expressions. A
   detailed description can be found in section 4, \"Meta-attributes\",
   of the Nixpkgs manual at <http://nixos.org/nixpkgs/docs.html>.
 -}

module Distribution.NixOS.Derivation.Meta
  ( Meta(..)
  , module Distribution.NixOS.Derivation.License
  )
  where

import Control.DeepSeq.Generics
import Distribution.NixOS.Derivation.License
import Distribution.NixOS.Util.PrettyPrinting
import GHC.Generics ( Generic )

-- | A representation of the @meta@ section used in Nix expressions.
--
-- >>> :{
--   putStrLn (display (Meta "http://example.org" "an example package" (Unknown Nothing)
--                      ["stdenv.lib.platforms."++x | x <- ["unix","cygwin"]]
--                      ["stdenv.lib.platforms.none"]
--                      ["joe","jane"]
--                      True))
-- :}
-- homepage = "http://example.org";
-- description = "an example package";
-- license = "unknown";
-- platforms = stdenv.lib.platforms.unix ++ stdenv.lib.platforms.cygwin;
-- hydraPlatforms = stdenv.lib.platforms.none;
-- maintainers = with self.stdenv.lib.maintainers; [ joe jane ];
-- broken = true;
--
-- Note that the "Text" instance definition provides pretty-printing,
-- but no parsing as of now!

data Meta = Meta
  { homepage       :: String    -- ^ URL of the package homepage
  , description    :: String    -- ^ short description of the package
  , license        :: License   -- ^ licensing terms
  , platforms      :: [String]  -- ^ list of supported platforms (from @pkgs\/lib\/platforms.nix@)
  , hydraPlatforms :: [String]  -- ^ list of platforms built by Hydra (from @pkgs\/lib\/platforms.nix@)
  , maintainers    :: [String]  -- ^ list of maintainers from @pkgs\/lib\/maintainers.nix@
  , broken         :: Bool      -- ^ set to @true@ if the build is known to fail
  }
  deriving (Show, Eq, Ord, Generic)

instance Pretty Meta where
  pPrint  = renderMeta

instance NFData Meta where rnf = genericRnf

renderMeta :: Meta -> Doc
renderMeta meta = vcat
  [ onlyIf (not (null (homepage meta))) $ attr "homepage" $ string (homepage meta)
  , onlyIf (not (null (description meta))) $ attr "description" $ string (description meta)
  , attr "license" $ pPrint (license meta)
  , onlyIf (not (null (platforms meta)) && platforms meta /= ["ghc.meta.platforms"]) $ sep
    [ text "platforms" <+> equals, renderPlatformList (platforms meta) ]
  , onlyIf (not (null (hydraPlatforms meta))) $ sep
    [ text "hydraPlatforms" <+> equals, renderPlatformList (hydraPlatforms meta) ]
  , listattr "maintainers" (text "with self.stdenv.lib.maintainers;") (maintainers meta)
  , boolattr "broken" (broken meta) (broken meta)
  ]

renderPlatformList :: [String] -> Doc
renderPlatformList plats =
  nest 2 (fsep $ punctuate (text " ++") $ map text plats) <> semi
