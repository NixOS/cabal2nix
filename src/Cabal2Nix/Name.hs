module Cabal2Nix.Name where

import Data.Char

toNixName :: String -> String
toNixName []      = error "toNixName: empty string is not a valid argument"
toNixName name    = f name
  where
    f []                            = []
    f ('-':c:cs) | c `notElem` "-"  = toUpper c : f cs
    f ('-':_)                       = error ("unexpected package name " ++ show name)
    f (c:cs)                        = c : f cs

-- | Map libraries to Nix packages.
-- TODO: This should probably be configurable. We also need to consider the
-- possibility of name clashes with Haskell libraries. I have included
-- identity mappings to incicate that I have verified their correctness.
libNixName :: String -> String
libNixName "adns"      = "adns"
libNixName "cairo"     = "cairo"
libNixName "cairo-pdf" = "cairo"
libNixName "cairo-ps"  = "cairo"
libNixName "cairo-svg" = "cairo"
libNixName "crypto"    = "openssl"
libNixName "gnome-keyring" = "gnome_keyring"
libNixName "gnome-keyring-1" = "gnome_keyring"
libNixName "idn"       = "idn"
libNixName "libidn"    = "idn"
libNixName "libzip"    = "libzip"
libNixName "pcre"      = "pcre"
libNixName "pq"        = "postgresql"
libNixName "sndfile"   = "libsndfile"
libNixName "sqlite3"   = "sqlite"
libNixName "xft"       = "libXft"
libNixName "X11"       = "libX11"
libNixName "z"         = "zlib"
libNixName x           = x
