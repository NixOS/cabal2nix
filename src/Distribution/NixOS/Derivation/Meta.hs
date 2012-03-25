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

import Distribution.NixOS.PrettyPrinting
import Distribution.NixOS.Derivation.License
import Distribution.Text

-- | A representation of the @meta@ section used in Nix expressions.
--
-- > > putStrLn (display (Meta "http://example.org" "an example package" (Unknown Nothing)
-- > >                   ["stdenv.lib.platforms."++x | x<-["unix","cygwin"]]
-- > >                   ["stdenv.lib.maintainers."++x | x<-["joe","jane"]]))
-- > meta = {
-- >   homepage = "http://example.org";
-- >   description = "an example package";
-- >   license = "unknown";
-- >   platforms =
-- >     stdenv.lib.platforms.unix ++ stdenv.lib.platforms.cygwin;
-- >   maintainers = [ stdenv.lib.maintainers.joe stdenv.lib.maintainers.jane ];
-- > };
--
-- Note that the "Text" instance definition provides pretty-printing,
-- but no parsing as of now!

data Meta = Meta
  { homepage    :: String       -- ^ URL of the package homepage
  , description :: String       -- ^ short description of the package
  , license     :: License      -- ^ licensing terms
  , platforms   :: [String]     -- ^ list of supported platforms from @pkgs\/lib\/platforms.nix@
  , maintainers :: [String]     -- ^ list of maintainers from @pkgs\/lib\/maintainers.nix@
  }
  deriving (Show, Eq, Ord)

instance Text Meta where
  disp  = renderMeta
  parse = error "parsing Distribution.NixOS.Derivation.Cabal.Meta is not supported yet"

renderMeta :: Meta -> Doc
renderMeta meta = vcat
  [ text "meta" <+> equals <+> lbrace
  , nest 2 $ vcat
    [ onlyIf (homepage meta) $ attr "homepage" $ string (homepage meta)
    , onlyIf (description meta) $ attr "description" $ string (description meta)
    , attr "license" $ disp (license meta)
    , onlyIf (platforms meta) $ sep
      [ text "platforms" <+> equals
      , nest 2 (fsep $ punctuate (text " ++") $ map text (platforms meta)) <> semi
      ]
    , listattr "maintainers" (maintainers meta)
    ]
  , rbrace <> semi
  ]
