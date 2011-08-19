module Cabal2Nix.Name ( toNixName, libNixName ) where

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
libNixName :: String -> [String]
libNixName "adns"               = return "adns"
libNixName "cairo-pdf"          = return "cairo"
libNixName "cairo-ps"           = return "cairo"
libNixName "cairo"              = return "cairo"
libNixName "cairo-svg"          = return "cairo"
libNixName "crypto"             = return "openssl"
libNixName "gnome-keyring-1"    = return "gnome_keyring"
libNixName "gnome-keyring"      = return "gnome_keyring"
libNixName "idn"                = return "idn"
libNixName "libidn"             = return "idn"
libNixName "libzip"             = return "libzip"
libNixName "m"                  = []  -- in stdenv
libNixName "pcre"               = return "pcre"
libNixName "pq"                 = return "postgresql"
libNixName "sndfile"            = return "libsndfile"
libNixName "sqlite3"            = return "sqlite"
libNixName "stdc++"             = []  -- in stdenv
libNixName "X11"                = return "libX11"
libNixName "xft"                = return "libXft"
libNixName "z"                  = return "zlib"
libNixName x                    = return x
