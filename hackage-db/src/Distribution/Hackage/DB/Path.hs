{- |
   Module      :  Distribution.Hackage.DB.Path
   License     :  BSD3
   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  portable

   Find the location of the local Hackage database that is maintained by running
   @cabal update@.
 -}

module Distribution.Hackage.DB.Path ( hackagePath ) where

import System.Directory ( getAppUserDataDirectory )
import System.FilePath ( joinPath )

-- | Determine the default path of the Hackage database, which typically
-- resides at @"$HOME\/.cabal\/packages\/hackage.haskell.org\/00-index.tar"@.
-- Running the command @"cabal update"@ will keep that file up-to-date.

hackagePath :: IO FilePath
hackagePath = do
  cabalDir <- getAppUserDataDirectory "cabal"
  return $ joinPath [cabalDir, "packages", "hackage.haskell.org", "00-index.tar"]
