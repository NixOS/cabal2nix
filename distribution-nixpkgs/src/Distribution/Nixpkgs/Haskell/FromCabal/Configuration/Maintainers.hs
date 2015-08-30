module Distribution.Nixpkgs.Haskell.FromCabal.Configuration.Maintainers ( globalPackageMaintainers ) where

import Data.Map as Map
import Data.Set as Set
import Distribution.Nixpkgs.Haskell.FromCabal.Configuration
import Control.Lens.Create

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
  , "jb55" |-> [ "pipes-csv", "pipes-mongodb", "cased" ]
  , "psibi" |-> [ "shakespeare", "path-pieces", "persistent", "persistent-mysql"
                , "persistent-postgresql", "persistent-sqlite", "persistent-redis"
                , "persistent-mongoDB", "persistent-template", "persistent-zookeeper"]
  ]

(|->) :: String -> [String] -> (Identifier,[PackageName])
(|->) m ps = (create ident m, Prelude.map PackageName ps)
