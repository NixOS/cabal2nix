module Distribution.Nixpkgs.Haskell.FromCabal.Configuration.Maintainers ( globalPackageMaintainers ) where

import Data.Map as Map
import Data.Set as Set
import Distribution.Nixpkgs.Haskell.FromCabal.Configuration
import Internal.Lens

globalPackageMaintainers :: Map PackageName (Set Identifier)
globalPackageMaintainers = Map.unionsWith Set.union [ Map.singleton p (Set.singleton m) | (m,ps) <- maintainedPackages, p <- ps ]

maintainedPackages :: [(Identifier, [PackageName])]
maintainedPackages =
  [ "simons" |->
       [ "cabal2nix", "funcmp", "hackage-db", "hledger-interest", "hopenssl"
       , "hsdns", "hsemail", "hsyslog", "jailbreak-cabal", "language-nix"
       , "streamproc"
       ]
  , "gebner" |-> [ "hledger-diff" ]
  ]

(|->) :: String -> [String] -> (Identifier,[PackageName])
(|->) m ps = (create ident m, Prelude.map PackageName ps)
