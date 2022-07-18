module Distribution.Nixpkgs.Haskell.Constraint
  ( Constraint, constraintPkgName, satisfiesConstraint, satisfiesConstraints
  ) where

import Distribution.Package
import Distribution.Version
import Distribution.Types.PackageVersionConstraint
import Distribution.Nixpkgs.Haskell.OrphanInstances ( )

type Constraint = PackageVersionConstraint

constraintPkgName :: Constraint -> PackageName
constraintPkgName (PackageVersionConstraint n _) = n

satisfiesConstraint :: PackageIdentifier -> Constraint -> Bool
satisfiesConstraint (PackageIdentifier pn v) (PackageVersionConstraint cn vr) = (pn /= cn) || (v `withinRange` vr)

satisfiesConstraints :: PackageIdentifier -> [Constraint] -> Bool
satisfiesConstraints p = all (satisfiesConstraint p)
