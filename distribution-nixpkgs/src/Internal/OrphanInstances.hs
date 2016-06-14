{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Internal.OrphanInstances ( ) where

import Control.DeepSeq
import Distribution.System

instance NFData Arch
instance NFData OS
instance NFData Platform
