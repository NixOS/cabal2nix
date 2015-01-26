{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, RecordWildCards #-}
{- |
   A representation of the @meta@ section used in Nix expressions. A
   detailed description can be found in section 4, \"Meta-attributes\",
   of the Nixpkgs manual at <http://nixos.org/nixpkgs/docs.html>.
 -}

module Distribution.Nixpkgs.Meta where

import Control.DeepSeq.Generics
import Control.Lens
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
  { _homepage       :: String      -- ^ URL of the package homepage
  , _description    :: String      -- ^ short description of the package
  , _license        :: License     -- ^ licensing terms
  , _platforms      :: Set String  -- ^ list of supported platforms (from @pkgs\/lib\/platforms.nix@)
  , _hydraPlatforms :: Set String  -- ^ list of platforms built by Hydra (from @pkgs\/lib\/platforms.nix@)
  , _maintainers    :: Set String  -- ^ list of maintainers from @pkgs\/lib\/maintainers.nix@
  , _broken         :: Bool        -- ^ set to @true@ if the build is known to fail
  }
  deriving (Show, Eq, Ord, Generic)

makeLenses ''Meta

instance NFData Meta where rnf = genericRnf

instance Pretty Meta where
  pPrint Meta {..} = vcat
    [ onlyIf (not (null _homepage)) $ attr "homepage" $ string _homepage
    , onlyIf (not (null _description)) $ attr "description" $ string _description
    , attr "license" $ pPrint _license
    , onlyIf (not (Set.null _platforms) && _platforms /= Set.singleton "ghc.meta.platforms") $ sep
      [ text "platforms" <+> equals, renderPlatformList (Set.toAscList _platforms) ]
    , onlyIf (not (Set.null _hydraPlatforms)) $ sep
      [ text "hydraPlatforms" <+> equals, renderPlatformList (Set.toAscList _hydraPlatforms) ]
    , listattr "maintainers" (text "with stdenv.lib.maintainers;") (Set.toAscList _maintainers)
    , boolattr "broken" _broken _broken
    ]

renderPlatformList :: [String] -> Doc
renderPlatformList plats =
  nest 2 (fsep $ punctuate (text " ++") $ map text plats) <> semi
