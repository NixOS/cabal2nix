{- |
   Module      :  Distribution.NixOS.Derivation.License
   License     :  BSD3

   Maintainer  :  nix-dev@cs.uu.nl
   Stability   :  provisional
   Portability :  portable

   Known licenses in Nix expressions are represented using the
   attributes defined in @pkgs\/lib\/licenses.nix@, and unknown licenses
   are represented as a literal string.
 -}

module Distribution.NixOS.Derivation.License ( License(..) ) where

import Distribution.NixOS.PrettyPrinting
import Distribution.Text

-- | The representation for licenses used in Nix derivations. Known
-- licenses are Nix expressions -- such as @stdenv.lib.licenses.bsd3@
-- --, so their exact \"name\" is not generally known, because the path
-- to @stdenv@ depends on the context defined in the expression. In
-- Cabal expressions, for example, the BSD3 license would have to be
-- referred to as @self.stdenv.lib.licenses.bsd3@. Other expressions,
-- however, use different paths to the @licenses@ record.. Because of
-- this station, this library cannot provide an abstract data type that
-- encompasses all known licenses. Instead, the @License@ type just
-- distinguishes references to known and unknown licenses. The
-- difference between the two is in the way they are pretty-printed:
--
-- > > putStrLn (display (Known "stdenv.lib.license.gpl2"))
-- > stdenv.lib.license.gpl2
-- >
-- > > putStrLn (display (Unknown (Just "GPL")))
-- > "GPL"
-- >
-- > > putStrLn (display (Unknown Nothing))
-- > "unknown"
--
-- Note that the "Text" instance definition provides pretty-printing,
-- but no parsing as of now!

data License = Known String
             | Unknown (Maybe String)
  deriving (Show, Eq, Ord)

instance Text License where
  disp (Known x)   = text x
  disp (Unknown x) = string $ maybe "unknown" id x
  parse = error "parsing Distribution.NixOS.Derivation.License is not supported yet"

