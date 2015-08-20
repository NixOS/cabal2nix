module Distribution.Nixpkgs.Haskell.FromCabal.CorePackages ( corePackages, coreBuildTools ) where

-- | List of packages shipped with ghc and therefore at the moment not in
-- nixpkgs. This should probably be configurable at first. Later, it might
-- be good to actually include them as dependencies, but set them to null
-- if GHC provides them (as different GHC versions vary).

corePackages :: [String]
corePackages = []

coreBuildTools :: [String]
coreBuildTools =
  [ "ghc"
  , "hsc2hs"
  ]
