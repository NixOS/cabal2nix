module Cabal2Nix.CorePackages ( corePackages ) where

-- | List of packages shipped with ghc and therefore at the moment not in
-- nixpkgs. This should probably be configurable at first. Later, it might
-- be good to actually include them as dependencies, but set them to null
-- if GHC provides them (as different GHC versions vary).
corePackages :: [String]
corePackages = [
    "array",
    "base",
    "bytestring",
    "Cabal",
    "containers",
    "directory",
    "extensible-exceptions",
    "filepath",
    "ghc-prim",
    "haskell2010",
    "haskell98",
    "hpc",
    "old-locale",
    "old-time",
    "pretty",
    "process",
    "random",
    "template-haskell",
    "time",
    "unix"
  ]
