module Cabal2Nix.Version where

import Data.Version (showVersion)
import Paths_cabal2nix (version)

version :: String
version = showVersion Paths_cabal2nix.version
