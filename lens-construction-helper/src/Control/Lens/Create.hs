module Control.Lens.Create ( create, Default(..), ASetter ) where

import Control.Lens.Setter ( ASetter, set )
import Data.Default.Class ( Default(..) )

-- | A convenient helper function for using Lenses as constructors.

create :: Default s => ASetter s t a b -> b -> t
create f x = set f x def
