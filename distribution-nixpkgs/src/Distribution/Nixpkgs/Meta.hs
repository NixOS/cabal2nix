{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

{- |
   A representation of the @meta@ section used in Nix expressions. A
   detailed description can be found in section 4, \"Meta-attributes\",
   of the Nixpkgs manual at <http://nixos.org/nixpkgs/docs.html>.
 -}

module Distribution.Nixpkgs.Meta
  ( Meta, nullMeta
  , homepage, description, license, platforms, hydraPlatforms, maintainers, broken
  ) where

-- Avoid name clash with Prelude.<> exported by post-SMP versions of base.
#if MIN_VERSION_base(4,11,0)
import Prelude hiding ( (<>) )
#endif
import Control.DeepSeq
import Control.Lens
import Data.Set ( Set )
import qualified Data.Set as Set
import Distribution.Nixpkgs.License
import Distribution.System
import GHC.Generics ( Generic )
import Language.Nix.Identifier
import Language.Nix.Path ( path )
import Language.Nix.PrettyPrinting

-- | A representation of the @meta@ section used in Nix expressions.
--
-- >>> :set -XOverloadedStrings
-- >>> :{
--   print (pPrint (Meta "http://example.org" "an example package" (Unknown Nothing)
--                  (Just (Set.singleton (Platform X86_64 Linux)))
--                  (Just Set.empty)
--                  (Set.fromList ["joe","jane"])
--                  True))
-- :}
-- homepage = "http://example.org";
-- description = "an example package";
-- license = "unknown";
-- platforms = [ "x86_64-linux" ];
-- hydraPlatforms = lib.platforms.none;
-- maintainers = [ lib.maintainers.jane lib.maintainers.joe ];
-- broken = true;

data Meta = Meta
  { _homepage       :: String
  -- ^ URL of the package homepage
  , _description    :: String
  -- ^ short description of the package
  , _license        :: License
  -- ^ licensing terms
  , _platforms      :: Maybe (Set Platform)
  -- ^ We re-use the Cabal type for convenience, but render it to conform to
  --   @pkgs\/lib\/platforms.nix@. 'Nothing' means we won't explicitly define it.
  , _hydraPlatforms :: Maybe (Set Platform)
  -- ^ list of platforms built by Hydra (render to conform to @pkgs\/lib\/platforms.nix@).
  --  'Nothing' means we won't explicitly define it.
  , _maintainers    :: Set Identifier
  -- ^ list of maintainers from @pkgs\/lib\/maintainers.nix@
  , _broken         :: Bool
  -- ^ set to @true@ if the build is known to fail
  }
  deriving (Show, Eq, Ord, Generic)

makeLenses ''Meta

instance NFData Meta

instance Pretty Meta where
  pPrint Meta {..} = vcat
    [ onlyIf (not (null _homepage)) $ attr "homepage" $ string _homepage
    , onlyIf (not (null _description)) $ attr "description" $ string _description
    , attr "license" $ pPrint _license
    , maybe mempty (renderPlatforms "platforms") _platforms
    , maybe mempty (renderPlatforms "hydraPlatforms") _hydraPlatforms
    , listattrDoc "maintainers" mempty $ renderMaintainers _maintainers
    , boolattr "broken" _broken _broken
    ]

renderPlatforms :: String -> Set Platform -> Doc
renderPlatforms field ps
  | Set.null ps = sep [ text field <+> equals <+> text "lib.platforms.none" <> semi ]
  | otherwise   = setattr field mempty $ Set.map fromCabalPlatform ps

renderMaintainers :: Set Identifier -> [Doc]
renderMaintainers = map (pPrint . toPath) . toAscListSortedOn (view ident)
  where toPath m = path # [ ident # "lib", ident # "maintainers", m]

nullMeta :: Meta
nullMeta = Meta
  { _homepage = error "undefined Meta.homepage"
  , _description = error "undefined Meta.description"
  , _license = error "undefined Meta.license"
  , _platforms = error "undefined Meta.platforms"
  , _hydraPlatforms = error "undefined Meta.hydraPlatforms"
  , _maintainers = error "undefined Meta.maintainers"
  , _broken = error "undefined Meta.broken"
  }

fromCabalPlatform :: Platform -> String
fromCabalPlatform (Platform I386 Linux)                 = "\"i686-linux\""
fromCabalPlatform (Platform X86_64 Linux)               = "\"x86_64-linux\""
fromCabalPlatform (Platform X86_64 OSX)                 = "\"x86_64-darwin\""
fromCabalPlatform (Platform (OtherArch "armv7l") Linux) = "\"armv7l-linux\""
fromCabalPlatform (Platform AArch64 Linux)              = "\"aarch64-linux\""
fromCabalPlatform (Platform AArch64 OSX)                = "\"aarch64-darwin\""
fromCabalPlatform p                                     = error ("fromCabalPlatform: invalid Nix platform" ++ show p)
