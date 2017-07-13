{- |
   Module      :  Distribution.Hackage.DB.Path
   License     :  BSD3
   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  portable

   Find the location of the local Hackage database that is maintained by running
   @cabal update@.
 -}

module Distribution.Hackage.DB.Path where

import Control.Monad
import System.Directory
import System.FilePath

-- | Determine the default path of the Hackage database, which typically
-- resides at @"$HOME\/.cabal\/packages\/hackage.haskell.org\/00-index.tar"@.
-- Running the command @"cabal update"@ will keep that file up-to-date.


cabalStateDir :: IO FilePath
cabalStateDir = getAppUserDataDirectory "cabal"

cabalTarballDir :: String -> IO FilePath
cabalTarballDir repo = do
  csd <- cabalStateDir
  return $ joinPath [csd, "packages", repo]

hackageTarballDir :: IO FilePath
hackageTarballDir = cabalTarballDir "hackage.haskell.org"

hackageTarball :: IO FilePath
hackageTarball = do
  htd <- hackageTarballDir
  let check p = doesFileExist p >>= \b -> if b then return p else fail p
  msum (map (check . (htd </>)) ["01-index.tar","00-index.tar"]) `mplus` fail "no tarball"
