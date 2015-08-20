module Internal.Lens
  ( create
  , module Data.Default
  , module Control.Lens
  ) where

import Control.Lens
import Data.Default

-- | A convenient helper function for using Lenses as constructors.

create :: Default s => ASetter s t a b -> b -> t
create f x = set f x def
