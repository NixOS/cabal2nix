module Distribution.Nixpkgs.Haskell.FromCabal.Platform ( fromCabalPlatform ) where

import Distribution.System

fromCabalPlatform :: Platform -> String
fromCabalPlatform (Platform I386 Linux)                     = "\"i686-linux\""
fromCabalPlatform (Platform X86_64 Linux)                   = "\"x86_64-linux\""
fromCabalPlatform (Platform X86_64 (OtherOS "darwin"))      = "\"x86_64-darwin\""
fromCabalPlatform p                                         = error ("fromCabalPlatform: invalid Nix platform" ++ show p)
