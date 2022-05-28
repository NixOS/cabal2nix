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
  ( -- * Representation of the Nixpkgs Meta Set
    Meta, nullMeta
    -- ** Lenses for 'Meta'
  , homepage, description, license, platforms, badPlatforms, hydraPlatforms, mainProgram, maintainers, broken
    -- * Representation of Nixpkgs Platform Descriptions
  , NixpkgsPlatform (..)
  , nixpkgsPlatformFromString
  ) where

-- Avoid name clash with Prelude.<> exported by post-SMP versions of base.
#if MIN_VERSION_base(4,11,0)
import Prelude hiding ( (<>) )
#endif
import Control.Applicative ( (<|>) )
import Control.DeepSeq
import Control.Lens hiding ( Strict )
import Data.List ( stripPrefix )
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
--       @i686-darwin@ and @armv7a-darwin@. Naturally, this is
--       subject to change as nixpkgs updates @lib.platforms@.
--     * 'NixpkgsPlatformSingle' indicates a single platform tuple
--       represented using Cabal's 'Platform'.
--
--   The former is useful to express related groups of
--   platforms which have similar properties. The latter
--   can be used to, for example, exclude a single, specific
--   platform.
--
--   @hackage2nix@ has used the latter approach historically
--   and is being extended to support nixpkgs' platform
--   groups as well for increased maintainer convenience.
--
--   The 'Pretty' instance allows for converting a 'NixpkgsPlatform'
--   into a Nix expression compatible with @meta.platforms@:
--
--   >>> pPrint $ NixpkgsPlatformSingle $ Platform X86_64 NetBSD
--   "x86_64-netbsd"
--
--   For 'NixpkgsPlatformGroup' we assume that the @lib@ attribute set is in
--   scope:
--
--   >>> pPrint $ NixpkgsPlatformGroup $ ident # "riscv"
--   lib.platforms.riscv
data NixpkgsPlatform
  = NixpkgsPlatformSingle Platform
  -- ^ Single platform represented as a Cabal platform. Can be understood as
  -- equivalent to Nix's system strings and will be converted to one usually.
  | NixpkgsPlatformGroup Identifier
  -- ^ 'Identifier' of the attribute name of a platform
  --   group in nixpkgs' @lib.platforms@.
  deriving (Show, Eq, Ord, Generic)

nixpkgsPlatformFromCabal :: Platform -> String
nixpkgsPlatformFromCabal (Platform arch os) = "\"" ++ nixArch ++ "-" ++ nixOs ++ "\""
  where nixArch =
          case arch of
            I386 -> "i686" -- rendered as i386 by default
            PPC -> "powerpc" -- rendered as ppc by default
            PPC64 -> "powerpc64" -- rendered as ppc64 by default
            JavaScript -> "js" -- rendered as javascript by default
            _    -> CabalPretty.prettyShow arch
        nixOs =
          case os of
            OSX -> "darwin" -- rendered as osx by default
            _   -> CabalPretty.prettyShow os

-- | Obtain a 'NixpkgsPlatform' from a string representation intended for config
--   files.
--
--   * Every string starting with @lib.platforms.@ or @platforms.@ is
--     parsed into 'NixpkgsPlatformGroup'.
--   * All other strings are attempted to be interpreted as a nix(pkgs) style
--     system tuple and parsed into 'NixpkgsPlatformSingle'.
--
--   If none of these formats match the input 'String', 'Nothing' is returned.
--   A 'Just' result thus only indicates that the format of the platform is
--   sound — 'nixpkgsPlatformFromString' does /not/ check if the parsed platform
--   actually exists.
--
--   'NixpkgsPlatformSingle' is parsed from system tuples as understood by Nix
--   and nixpkgs. System tuples are derived from autoconf's
--   [target triplets](https://www.gnu.org/savannah-checkouts/gnu/autoconf/manual/autoconf-2.70/autoconf.html#Manual-Configuration),
--   dropping the vendor part. They have the form @cpu-os@ where @os@ can either
--   be a single component or of the form @kernel-system@ (system is an autoconf
--   term here, not a Nix system). Note that three component systems are very
--   rare. The two main candidates @x86_64-linux-musl@ and @x86_64-linux-gnu@
--   are [prohibited for historical reasons](https://github.com/NixOS/nix/blob/ec07a70979a86cc436de7e46e03789b4606d25ab/configure.ac#L26-L28)
--   and represented as plain @x86_64-linux@ instead.
--
--   Note that 'nixpkgsPlatformFromString' expects to receive a /valid/ system
--   tuple, i.e. it will accept all system tuples that have a sound format
--   (with the caveat that it will accept n-tuples for @n >= 4@ even though
--   they are technically invalid). This is done because the ambiguity of
--   system tuples requires knowledge over its legal contents in order to check
--   their validity properly. Since @lib.systems.elaborate@ from nixpkgs is the
--   source of truth in this case, we want to avoid the need to continuously
--   update @distribution-nixpkgs@ to reflect its inner workings.
--
--   'nixpkgsPlatformFromString' does, however, some conversions to alleviate some
--   discrepancies between Cabal and nixpkgs. Parsing and rendering system tuples
--   using 'nixpkgsPlatformFromString' and rendering them via the 'Pretty'
--   instance of 'NixpkgsPlatform' should not change the system tuple
--   for tuples accepted by nixpkgs. This has been tested for all known tuples
--   (from @lib.platforms@ and @lib.systems.examples@) as of 2022-05-08.
--   Please open an issue if any newly added ones are not recognized properly.
--
--   __Warning__: 'nixpkgsPlatformFromString' consequently tries to preserve all
--   information of the passed system tuple. This means that it distinguishes
--   between things that Cabal wouldn't, e.g. `powerpc64` and `powerpc64le`. If
--   you use this function to obtain a 'Platform' that is later used to evaluate
--   a @.cabal@ file, it will behave unexpectedly in some situation. It is
--   recommended to use Cabal's own facilities or
--   @Distribution.Nixpkgs.Haskell.Platform@, provided by @cabal2nix@, instead.
--
--   'nixpkgsPlatformFromString' is also /not/ the inverse operation for
--   'NixpkgsPlatform'\'s 'Pretty' instance. It is not intended for parsing Nix
--   expressions.
--
--   >>> nixpkgsPlatformFromString "x86_64-netbsd"
--   Just (NixpkgsPlatformSingle (Platform X86_64 NetBSD))
--   >>> nixpkgsPlatformFromString "platforms.riscv"
--   Just (NixpkgsPlatformGroup (Identifier "riscv"))
--   >>> nixpkgsPlatformFromString "garbage"
--   Nothing
nixpkgsPlatformFromString :: String -> Maybe NixpkgsPlatform
nixpkgsPlatformFromString s = platformGroup <|> singlePlatform
  where platformGroup = do
          -- also accept "platform." as prefix to save some typing
          name <- stripPrefix "lib.platforms." s <|> stripPrefix "platforms." s
          Just $ NixpkgsPlatformGroup (ident # name)

        singlePlatform = NixpkgsPlatformSingle <$> cabalPlatformFromSystem s

-- | Parse a system tuple as understood by Nix and nixpkgs to a Cabal 'Platform'.
--   Used internally by 'nixpkgsPlatformFromString'.
--
--   >>> cabalPlatformFromSystem "x86_64-linux"
--   Just (Platform X86_64 Linux)
--   >>> cabalPlatformFromSystem "x86_64-linux-musl"
--   Just (Platform X86_64 (OtherOS "linux-musl"))
--   >>> cabalPlatformFromSystem "powerpc-darwin"
--   Just (Platform PPC OSX)
--   >>> cabalPlatformFromSystem "powerpc64le-linux"
--   Just (Platform (OtherArch "powerpc64le") Linux)
--   >>> cabalPlatformFromSystem "js-ghcjs"
--   Just (Platform JavaScript Ghcjs)
cabalPlatformFromSystem :: String -> Maybe Platform
cabalPlatformFromSystem s =
  case break (== '-') s of
    (arch, '-':os) ->
      if null arch || null os
      then Nothing
      else Just $ Platform (parseArch arch) (parseOS os)
    _ -> Nothing
  where -- Use permissive classification to also recognize autoconf / nixpkgs
        -- style OS strings, e.g. "darwin" where Cabal would expect "osx".
        --
        -- Note that we don't reimplement GHC_CONVERT_OS from GHC's configure
        -- file here at the moment. Our goal is to recognize all /well formed/
        -- nixpkgs system strings, nothing more. That this works correctly for
        -- nixpkgs systems currently in use is confirmed by the test suite.
        parseOS = classifyOS Permissive

        -- Use Strict for arch classification and add specific guards for cases
        -- where Cabal's naming expectations (which seems to be LLVM oriented)
        -- clash with what nixpkgs / autoconf use (e.g. Cabal uses "ppc" instead
        -- of "powerpc"). Using Permissive is not possible because Cabal is a bit
        -- overzealous in arch recognition: For example, it ignores endianess in
        -- the case of the POWER architectures, parsing "powerpcle" to "ppc" and
        -- "powerpc64le" to "ppc64" when they are clearly different platforms.
        --
        -- We also don't implement GHC_CONVERT_ARCH here, for the reasons stated
        -- above.
        parseArch as =
          case classifyArch Strict as of
            OtherArch "i686" -> I386
            OtherArch "js" -> JavaScript
            OtherArch "powerpc" -> PPC
            OtherArch "powerpc64" -> PPC64
            a -> a

instance Pretty NixpkgsPlatform where
  pPrint (NixpkgsPlatformSingle p) = text $ nixpkgsPlatformFromCabal p
  pPrint (NixpkgsPlatformGroup p)  = pPrint
    $ path # [ ident # "lib", ident # "platforms", p ]

instance NFData NixpkgsPlatform

-- | A representation of the @meta@ section used in Nix expressions.
--
-- >>> :set -XOverloadedStrings
-- >>> :{
--   let meta = nullMeta
--         & homepage .~ "http://example.org"
--         & description .~ "An example package"
--         & license .~ Unknown Nothing
--         & platforms .~ Just (Set.singleton (NixpkgsPlatformSingle (Platform X86_64 Linux)))
--         & badPlatforms .~ Nothing
--         & hydraPlatforms .~ Just Set.empty
--         & mainProgram .~ Just "example-binary"
--         & maintainers .~ Set.fromList ["joe", "jane"]
--         & broken .~ True
--   in print $ pPrint meta
-- :}
-- homepage = "http://example.org";
-- description = "An example package";
-- license = "unknown";
-- platforms = [ "x86_64-linux" ];
-- hydraPlatforms = lib.platforms.none;
-- mainProgram = "example-binary";
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
  , _mainProgram    :: Maybe String
  -- ^ Filename (as in basename) of the main executable provided by the described
  --   package. @Nothing@ if it is a library or no obvious default can be chosen.
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
    , maybe mempty (attr "mainProgram" . string) _mainProgram
    , listattrDoc "maintainers" mempty $ renderMaintainers _maintainers
    , boolattr "broken" _broken _broken
    ]

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
--   * First we render all 'NixpkgsPlatformSingle' values. This is done by
--     printing a Nix list containing Nix system strings. If there are no
--     platform groups, but the 'Set' is not empty, an empty list (@[ ]@) is
--     rendered.
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
--     "i686-linux" "x86_64-linux" "x86_64-darwin" "aarch64-linux"
--     "aarch64-darwin"
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
--     "i686-linux" "x86_64-linux" "x86_64-darwin" "aarch64-linux"
--     "aarch64-darwin"
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
        (cabalPs, nixpkgsPs) = Set.partition isSinglePlatform ps
        isSinglePlatform (NixpkgsPlatformSingle _) = True
        isSinglePlatform _ = False

        renderedCabalPs = map pPrint $ Set.toAscList cabalPs

        -- append lib.platforms list via nix's ++ at the end
        -- if there is no cabal platforms list, don't emit leading ++
        appendNixpkgsP acc elem = acc $$
          if isEmpty acc && Set.null cabalPs
          then nest 3 $ pPrint elem
          else text "++" <+> pPrint elem
        renderedNixpkgsPs = Set.foldl' appendNixpkgsP mempty nixpkgsPs

        -- Helper function, roughly the inverse of nixpkgs' optionals
        unless False x = x
        unless True _  = mempty

renderMaintainers :: Set Identifier -> [Doc]
renderMaintainers = map (pPrint . toPath) . toAscListSortedOn (view ident)
  where toPath m = path # [ ident # "lib", ident # "maintainers", m]

-- | 'Meta' record with no field set, i.e. evaluating any will throw:
--
--   >>> nullMeta ^. homepage
--   "*** Exception: undefined Meta.homepage
--   ...
nullMeta :: Meta
nullMeta = Meta
  { _homepage = error "undefined Meta.homepage"
  , _description = error "undefined Meta.description"
  , _license = error "undefined Meta.license"
  , _platforms = error "undefined Meta.platforms"
  , _badPlatforms = error "undefined Meta.badPlatforms"
  , _hydraPlatforms = error "undefined Meta.hydraPlatforms"
  , _mainProgram = error "undefined Meta.mainProgram"
  , _maintainers = error "undefined Meta.maintainers"
  , _broken = error "undefined Meta.broken"
  }
