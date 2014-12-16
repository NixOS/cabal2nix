{- |
   Module      :  Distribution.NixOS.Derivation.Meta
   License     :  BSD3

   Maintainer  :  nix-dev@cs.uu.nl
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

import Control.DeepSeq
import Distribution.NixOS.Derivation.License
import Distribution.NixOS.PrettyPrinting
import Distribution.Text

-- | A representation of the @meta@ section used in Nix expressions.
--
-- >>> :{
--   putStrLn (display (Meta "http://example.org" "an example package" (Unknown Nothing)
--                      ["stdenv.lib.platforms."++x | x <- ["unix","cygwin"]]
--                      ["stdenv.lib.platforms.none"]
--                      ["joe","jane"]))
-- :}
-- meta = {
--   homepage = "http://example.org";
--   description = "an example package";
--   license = "unknown";
--   platforms = stdenv.lib.platforms.unix ++ stdenv.lib.platforms.cygwin;
--   hydraPlatforms = stdenv.lib.platforms.none;
--   maintainers = with self.stdenv.lib.maintainers; [ joe jane ];
-- };
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
  deriving (Show, Eq, Ord)

instance Text Meta where
  disp  = renderMeta
  parse = error "parsing Distribution.NixOS.Derivation.Cabal.Meta is not supported yet"

instance NFData Meta where
  rnf (Meta a b c d e f g) = a `deepseq` b `deepseq` c `deepseq` d `deepseq` e `deepseq` f `deepseq` g `deepseq` ()

renderMeta :: Meta -> Doc
renderMeta meta = vcat
  [ onlyIf (not (null (homepage meta))) $ attr "homepage" $ string (homepage meta)
  , onlyIf (not (null (description meta))) $ attr "description" $ string (description meta)
  , attr "license" $ disp (license meta)
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
