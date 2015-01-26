{-# LANGUAGE DeriveGeneric, RecordWildCards #-}
{- |
   A representation of the @meta@ section used in Nix expressions. A
   detailed description can be found in section 4, \"Meta-attributes\",
   of the Nixpkgs manual at <http://nixos.org/nixpkgs/docs.html>.
 -}

module Distribution.Nixpkgs.Meta
  ( Meta(..)
  , module Distribution.Nixpkgs.License
  )
  where

import Control.DeepSeq.Generics
import Distribution.Nixpkgs.License
import Distribution.Nixpkgs.Util.PrettyPrinting
import GHC.Generics ( Generic )
import Data.Set ( Set )
import qualified Data.Set as Set

-- | A representation of the @meta@ section used in Nix expressions.
--
-- >>> :{
--   print (pPrint (Meta "http://example.org" "an example package" (Unknown Nothing)
--                  (Set.singleton "stdenv.lib.platforms.unix")
--                  (Set.singleton "stdenv.lib.platforms.none")
--                  (Set.fromList ["joe","jane"])
--                  True))
-- :}
-- homepage = "http://example.org";
-- description = "an example package";
-- license = "unknown";
-- platforms = stdenv.lib.platforms.unix;
-- hydraPlatforms = stdenv.lib.platforms.none;
-- maintainers = with stdenv.lib.maintainers; [ jane joe ];
-- broken = true;
--
-- Note that the "Text" instance definition provides pretty-printing,
-- but no parsing as of now!

data Meta = Meta
  { homepage       :: String      -- ^ URL of the package homepage
  , description    :: String      -- ^ short description of the package
  , license        :: License     -- ^ licensing terms
  , platforms      :: Set String  -- ^ list of supported platforms (from @pkgs\/lib\/platforms.nix@)
  , hydraPlatforms :: Set String  -- ^ list of platforms built by Hydra (from @pkgs\/lib\/platforms.nix@)
  , maintainers    :: Set String  -- ^ list of maintainers from @pkgs\/lib\/maintainers.nix@
  , broken         :: Bool        -- ^ set to @true@ if the build is known to fail
  }
  deriving (Show, Eq, Ord, Generic)

instance Pretty Meta where
  pPrint  = renderMeta

instance NFData Meta where rnf = genericRnf

renderMeta :: Meta -> Doc
renderMeta (Meta {..}) = vcat
  [ onlyIf (not (null homepage)) $ attr "homepage" $ string homepage
  , onlyIf (not (null description)) $ attr "description" $ string description
  , attr "license" $ pPrint license
  , onlyIf (not (Set.null platforms) && platforms /= Set.singleton "ghc.meta.platforms") $ sep
    [ text "platforms" <+> equals, renderPlatformList (Set.toAscList platforms) ]
  , onlyIf (not (Set.null hydraPlatforms)) $ sep
    [ text "hydraPlatforms" <+> equals, renderPlatformList (Set.toAscList hydraPlatforms) ]
  , listattr "maintainers" (text "with stdenv.lib.maintainers;") (Set.toAscList maintainers)
  , boolattr "broken" broken broken
  ]

renderPlatformList :: [String] -> Doc
renderPlatformList plats =
  nest 2 (fsep $ punctuate (text " ++") $ map text plats) <> semi
