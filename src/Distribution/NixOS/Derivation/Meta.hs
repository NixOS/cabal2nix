{- |
   Module      :  Distribution.NixOS.Derivation.Meta
   Copyright   :  Peter Simons, Andres Loeh
   License     :  BSD3

   Maintainer  :  nix-dev@cs.uu.nl
   Stability   :  provisional
   Portability :  portable
-}

module Distribution.NixOS.Derivation.Meta
  ( Meta(..)
  , module Distribution.NixOS.Derivation.License
  )
  where

import Distribution.NixOS.PrettyPrinting
import Distribution.NixOS.Derivation.License
import Distribution.Text

data Meta = Meta
  { homepage    :: String
  , description :: String
  , license     :: License
  , platforms   :: [String]
  , maintainers :: [String]
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
      , nest 2 ((fsep $ punctuate (text " ++") $ map text (platforms meta))) <> semi
      ]
    , listattr "maintainers" (maintainers meta)
    ]
  , rbrace <> semi
  ]
