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

-- Some logic to handle both old ~/.cabal and XDG cache directory used
-- by default from cabal-install 3.10.1.0.
--
-- This is a simplified form of the logic found in cabal-install
-- itself:
-- https://github.com/haskell/cabal/blob/0ed12188525335ac9759dc957d49979ab09382a1/cabal-install/src/Distribution/Client/Config.hs#L594-L610
cabalStateDir :: IO FilePath
cabalStateDir = do
  dotCabal <- getAppUserDataDirectory "cabal"
  dotCabalExists <- doesDirectoryExist dotCabal
  if dotCabalExists then pure dotCabal else
    getXdgDirectory XdgCache "cabal"

cabalTarballDir :: String -> IO FilePath
cabalTarballDir repo = do
  csd <- cabalStateDir
  return $ joinPath [csd, "packages", repo]

hackageTarballDir :: IO FilePath
hackageTarballDir = cabalTarballDir "hackage.haskell.org"

-- | Determine the default path of the Hackage database, which typically
-- resides at @"$HOME\/.cabal\/packages\/hackage.haskell.org\/00-index.tar"@.
-- Running the command @"cabal update"@ will keep that file up-to-date.

hackageTarball :: IO FilePath
hackageTarball = do
  htd <- hackageTarballDir
  let idx00 = htd </> "00-index.tar"
      idx01 = htd </> "01-index.tar"
  -- Using 'msum' here would be nice, but unfortunetaly there was no reliable
  -- MonadPlus instance for IO in pre 8.x versions of GHC. So we use the ugly
  -- code for sake of portability.
  have01 <- doesFileExist idx01
  if have01 then return idx01 else do
    have00 <- doesFileExist idx00
    if have00 then return idx00 else
      throwIO NoHackageTarballFound
