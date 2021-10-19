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
  , NixpkgsPlatform (..)
  , homepage, description, license, platforms, badPlatforms, hydraPlatforms, maintainers, broken
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
import qualified Distribution.Pretty as CabalPretty
import Distribution.System
import GHC.Generics ( Generic )
import Language.Nix.Identifier
import Language.Nix.Path ( path )
import Language.Nix.PrettyPrinting

-- | Representation of platform(s) as supported by nixpkgs:
--
--     * 'NixpkgsPlatformGroup' represents the name of a platform
--       list as found in @lib.platforms@. For example, at the
--       time of writing @NixpkgsPlatformGroup "darwin"@ would
--       represent the platform tuples @x86_64-darwin@, @aarch64-darwin@,
--       @i686-darwin@ and @armv7a-darwin@. Of course this is
--       subject to change as nixpkgs updates @lib.platforms@,
--       of course.
--     * 'NixpkgsPlatformSingle' indicates a single platform tuple
--       represented using Cabal's 'Platform'.
--
--   The former is useful to express related groups of
--   platforms which have similar properties. The latter
--   can be used to be more precise and to exclude a
--   single, specific platform.
--
--   @hackage2nix@ has used the latter approach historically
--   and is being extended to support nixpkgs' platform
--   groups as well for increased maintainer convenience.
data NixpkgsPlatform
  = NixpkgsPlatformSingle Platform
  -- ^ Single platform represented as a Cabal platform. Can be understood as
  -- equivalent to Nix's system strings and will be converted to one usually.
  | NixpkgsPlatformGroup Identifier
  -- ^ 'Identifier' of the attribute name of a platform
  --   group in nixpkgs' @lib.platforms@.
  deriving (Show, Eq, Ord, Generic)

instance NFData NixpkgsPlatform

-- | A representation of the @meta@ section used in Nix expressions.
--
-- >>> :set -XOverloadedStrings
-- >>> :{
--   print (pPrint (Meta "http://example.org" "an example package" (Unknown Nothing)
--                  (Just (Set.singleton (NixpkgsPlatformSingle (Platform X86_64 Linux))))
--                  Nothing
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
  , _platforms      :: Maybe (Set NixpkgsPlatform)
  -- ^ List of platforms that are supported by the package.
  --   'Nothing' prevents the attribute from being rendered.
  --   See 'NixpkgsPlatform' on the precise representation of platforms.
  , _badPlatforms   :: Maybe (Set NixpkgsPlatform)
  -- ^ List of platforms that are known to be unsupported. This is semantically
  --   equivalent to setting the following:
  --
  --   @
  --     platforms = lib.subtractLists
  --       (initialMeta.badPlatforms or []);
  --       (initialMeta.platforms or lib.platforms.all)
  --   @
  --
  --   'Nothing' prevents the attribute from being rendered.
  --   See 'NixpkgsPlatform' on the precise representation of platforms.
  , _hydraPlatforms :: Maybe (Set NixpkgsPlatform)
  -- ^ Platforms for which the package should be tested, built and added to the
  --   binary cache by Hydra. 'Nothing' prevents the attribute from being rendered.
  --  See 'NixpkgsPlatform' on the precise representation of platforms.
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
    , maybe mempty (renderPlatforms "badPlatforms") _badPlatforms
    , maybe mempty (renderPlatforms "hydraPlatforms") _hydraPlatforms
    , listattrDoc "maintainers" mempty $ renderMaintainers _maintainers
    , boolattr "broken" _broken _broken
    ]

partitionPlatforms :: Set NixpkgsPlatform -> (Set Platform, Set Identifier)
partitionPlatforms s =
  ( Set.map fromSingle singles
  , Set.map fromGroup groups
  )
  where (singles, groups) = Set.partition isSingle s
        isSingle p = case p of
          NixpkgsPlatformSingle _ -> True
          _ -> False
        fromSingle p = case p of
          NixpkgsPlatformSingle x -> x
          _ -> error "fromSingle: not single"
        fromGroup p = case p of
          NixpkgsPlatformGroup x -> x
          _ -> error "fromGroup: not group"

-- | This function renders an Nix attribute binding suitable for use in
--   an attribute set representing the given set of 'NixpkgsPlatform's.
--
--   The @field@ argument is the name of the binding, usually either
--   @platforms@, @hydraPlatforms@ or @badPlatforms@.
--
--   Platforms are rendered in the following way:
--
--   * If the given 'Set' is empty, the binding's value is @lib.platforms.none@.
--     This has been preserved for “backwards compatibility” since changing this
--     would generate a huge diff for nixpkgs' @hackage-packages.nix@ file.
--
--   * First we render all 'NixpkgsPlatformSingle' values. This is done
--     by printing a Nix list containing Nix system strings which are obtained
--     using 'fromCabalPlatform'. If there are no platform groups, but the 'Set'
--     is not empty, an empty list (@[ ]@) is rendered.
--
--   * Finally we render all 'NixpkgsPlatformGroup's. Since these are lists, we
--     need to append them to the (cabal) platform list via the @++@ operator.
--     For each group we render @++ lib.platforms.<group name>@.
--
--   For example:
--
--   >>> :{
--     renderPlatforms "badPlatforms"
--       $ Set.fromList [
--         NixpkgsPlatformSingle (Platform (OtherArch "armv7l") Linux),
--         NixpkgsPlatformGroup (ident # "darwin"),
--         NixpkgsPlatformGroup (ident # "windows")
--       ]
--   :}
--   badPlatforms = [
--     "armv7l-linux"
--   ] ++ lib.platforms.darwin
--     ++ lib.platforms.windows;
--
--   >>> :{
--     renderPlatforms "platforms"
--       $ Set.fromList [
--         NixpkgsPlatformGroup (ident # "x86"),
--         NixpkgsPlatformGroup (ident # "riscv"),
--         NixpkgsPlatformGroup (ident # "freebsd")
--       ]
--   :}
--   platforms =
--        lib.platforms.freebsd
--     ++ lib.platforms.riscv
--     ++ lib.platforms.x86;
--
--   >>> :{
--     renderPlatforms "platforms"
--       $ Set.fromList [
--         NixpkgsPlatformGroup (ident # "x86")
--       ]
--   :}
--   platforms = lib.platforms.x86;
--
--   >>> :{
--     renderPlatforms "platforms"
--       $ Set.fromList [
--         NixpkgsPlatformSingle (Platform I386 Linux),
--         NixpkgsPlatformSingle (Platform X86_64 Linux),
--         NixpkgsPlatformSingle (Platform AArch64 Linux),
--         NixpkgsPlatformSingle (Platform X86_64 OSX),
--         NixpkgsPlatformSingle (Platform AArch64 OSX)
--       ]
--   :}
--   platforms = [
--     "aarch64-darwin" "aarch64-linux" "i686-linux" "x86_64-darwin"
--     "x86_64-linux"
--   ];
--
--   >>> :{
--     renderPlatforms "platforms"
--       $ Set.fromList [
--         NixpkgsPlatformSingle (Platform I386 Linux),
--         NixpkgsPlatformSingle (Platform X86_64 Linux),
--         NixpkgsPlatformSingle (Platform AArch64 Linux),
--         NixpkgsPlatformSingle (Platform X86_64 OSX),
--         NixpkgsPlatformSingle (Platform AArch64 OSX),
--         NixpkgsPlatformGroup (ident # "riscv"),
--         NixpkgsPlatformGroup (ident # "arm")
--       ]
--   :}
--   platforms = [
--     "aarch64-darwin" "aarch64-linux" "i686-linux" "x86_64-darwin"
--     "x86_64-linux"
--   ] ++ lib.platforms.arm
--     ++ lib.platforms.riscv;
renderPlatforms :: String -> Set NixpkgsPlatform -> Doc
renderPlatforms field ps
  -- preserve old behavior: no platforms -> lib.platforms.none
  | Set.null ps = sep [ text field <+> equals <+> text "lib.platforms.none" <> semi ]
  | otherwise =
    sep [ text field <+> equals <+> unless (Set.null cabalPs) lbrack
        , unless (Set.null cabalPs) (nest 2 $ fsep renderedCabalPs)
        , unless (Set.null cabalPs) rbrack
        , unless (Set.null nixpkgsPs) (nest 2 renderedNixpkgsPs)
        ]
    <> semi
  where -- render nixpkgs platforms and cabal platform tuples separately
        -- since the former represents multiple platforms and meta doesn't
        -- support nested lists.
        (cabalPs, nixpkgsPs) = partitionPlatforms ps

        renderedCabalPs = map text $ Set.toAscList $ Set.map fromCabalPlatform cabalPs

        -- render a lib.platform
        platformPath p = pPrint $ path # [ ident # "lib", ident # "platforms", p ]
        -- append lib.platforms list via nix's ++ at the end
        -- if there is no cabal platforms list, don't emit leading ++
        appendNixpkgsP acc elem = acc $$
          if isEmpty acc && Set.null cabalPs
          then nest 3 $ platformPath elem
          else text "++" <+> platformPath elem
        renderedNixpkgsPs = Set.foldl' appendNixpkgsP mempty nixpkgsPs

        -- Helper function, roughly the inverse of nixpkgs' optionals
        unless False x = x
        unless True _  = mempty

renderMaintainers :: Set Identifier -> [Doc]
renderMaintainers = map (pPrint . toPath) . toAscListSortedOn (view ident)
  where toPath m = path # [ ident # "lib", ident # "maintainers", m]

nullMeta :: Meta
nullMeta = Meta
  { _homepage = error "undefined Meta.homepage"
  , _description = error "undefined Meta.description"
  , _license = error "undefined Meta.license"
  , _platforms = error "undefined Meta.platforms"
  , _badPlatforms = error "undefined Meta.badPlatforms"
  , _hydraPlatforms = error "undefined Meta.hydraPlatforms"
  , _maintainers = error "undefined Meta.maintainers"
  , _broken = error "undefined Meta.broken"
  }

fromCabalPlatform :: Platform -> String
fromCabalPlatform (Platform arch os) = "\"" ++ nixArch ++ "-" ++ nixOs ++ "\""
  where nixArch =
          case arch of
            I386 -> "i686" -- rendered as i386 by default
            _    -> CabalPretty.prettyShow arch
        nixOs =
          case os of
            OSX -> "darwin" -- rendered as osx by default
            _   -> CabalPretty.prettyShow os
