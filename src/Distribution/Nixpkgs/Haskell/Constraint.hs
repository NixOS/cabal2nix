module Distribution.Nixpkgs.Haskell.Constraint
  ( Constraint, satisfiesConstraint, satisfiesConstraints
  ) where

import Distribution.Package
import Distribution.Version

type Constraint = Dependency

satisfiesConstraint :: PackageIdentifier -> Constraint -> Bool
satisfiesConstraint (PackageIdentifier pn v) (Dependency cn vr) = (pn /= cn) || (v `withinRange` vr)

satisfiesConstraints :: PackageIdentifier -> [Constraint] -> Bool
satisfiesConstraints p = all (satisfiesConstraint p)
