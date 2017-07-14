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

import Distribution.Hackage.DB.Errors

import Control.Exception
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
  let idx00 = htd </> "00-index.tar"
      idx01 = htd </> "01-index.tar"
  -- Yes, using 'msum' here instead of this convolted mess would be nice, but
  -- unfortunetaly there was no reliable MonadPlus instance for IO in pre 8.x
  -- versions of GHC. Se we use the ugly code for sake of portability.
  have01 <- doesFileExist idx01
  if have01 then return idx01 else do
    have00 <- doesFileExist idx00
    if have00 then return idx00 else
      throwIO NoHackageTarballFound
